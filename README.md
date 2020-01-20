# bf-haskell

## Overview

`bf-haskell` is a Haskell library that implements a client for
[Betfair Exchange API](https://docs.developer.betfair.com/display/1smk3cen4v3lu3yomq5qye0ni/API+Overview).

It uses _free monad_ library [Polysemy](https://github.com/polysemy-research/polysemy).

## Example

Below is an example program, that looks up next horse race, waits for it to go live,
get closed, and then prints the winner of the race.

It uses:

* [Betting API](http://docs.developer.betfair.com/display/1smk3cen4v3lu3yomq5qye0ni/Betting+API)
* [Heartbeat API](https://docs.developer.betfair.com/display/1smk3cen4v3lu3yomq5qye0ni/Heartbeat+API)
* [Exchange Stream API](https://docs.developer.betfair.com/display/1smk3cen4v3lu3yomq5qye0ni/Exchange+Stream+API)

```haskell
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           BfHaskell.DSL.Betting
import           BfHaskell.DSL.Heartbeat
import           BfHaskell.DSL.Login
import           BfHaskell.DSL.Streaming
import           BfHaskell.StreamingAPI.Model
import           Control.Lens
import           Control.Monad                (forM_, forever, when)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Map                     as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time                    (addUTCTime, getCurrentTime)
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Resource
import           Polysemy.State

type FullMarketDetails = (MarketId, JsonMarketCatalogue)

data AppState = Initial
              | HasRaceInFuture FullMarketDetails
              | RaceInPlay FullMarketDetails


-- | Finds next horse race going in-play
findNextRace :: Members '[Embed IO, BettingHandler] r
             => Text   -- ^ Event type id
             -> Sem r (Maybe JsonMarketCatalogue)
findNextRace eventTypeId = do
    currentTime <- liftIO getCurrentTime

    let dateUntil = addUTCTime (60 * 60 * 24 * 5) currentTime
        marketFilter = createMarketFilter eventTypeId
                                          (Just currentTime)
                                          (Just dateUntil)
                                          Nothing -- No competition specified
    JsonResponseListMarketCatalogue markets <- listMarkets marketFilter
    return $ markets ^? folded
                -- Only race in future
                . filteredBy (to _jmcatMarketStartTime . _Just . filtered (>currentTime))
                -- Only races going in play with type ODDS
                . filteredBy (to _jmcatDescription . _Just . filtered validMarketDescription)

  where
    validMarketDescription description =
        _jmdTurnInPlayEnabled description && _jmdBettingType description == ODDS

-- | Updates state according to market changes
processMarketUpdate :: Members '[Embed IO, StreamingHandler, BettingHandler, State AppState, Error String] r
                    => [MarketId]
                    -> M.Map MarketId MarketDetails
                    -> Sem r ()
processMarketUpdate marketIds cache = do
    state <- get
    case state of
        Initial -> return ()

        -- We are waiting for upcoming race to go live.
        -- If it went live, transition to next state: RaceInPlay
        HasRaceInFuture (marketId, market) ->
            -- Find actual market details from cache
            forM_ (M.lookup marketId cache) $ \md -> do
                let inPlay = mdMarketDefinition . _Just . to marketDefinitionInPlay
                                                . _Just . only True
                -- Act only if there are changes on expected marketId
                when (elem marketId marketIds && has inPlay md) $ do
                    liftIO $ putStrLn $ T.unpack $ "Race in play: " <> _jmcatMarketName market
                    put $ RaceInPlay (marketId, market)

        -- Game is in play. Waiting for game to be closed
        -- Next state: HasRaceInFuture
        RaceInPlay (marketId, market) ->
            case M.lookup marketId cache of
                Just md -> do
                    let winner = md ^? mdMarketDefinition . _Just . to marketDefinitionRunners
                                    . _Just . folded
                                    . filteredBy (to runnerDefinitionStatus . _Just . only E'Status2'WINNER)
                    forM_ winner $ \rd -> do
                        forM_ (findRunnerName market $ runnerDefinitionId rd) $ \winnerName ->
                            liftIO $ putStrLn $ T.unpack $ "Race winner: " <> winnerName

                        subsribeToNextGame

                -- Market already removed from cache. Move on to next race
                Nothing -> do
                    liftIO $ putStrLn $ T.unpack $ "Race is closed: " <> _jmcatMarketName market
                    subsribeToNextGame
  where
    -- | Find runner name in market catalogue
    findRunnerName market (Just runnerId) =
        market ^? to _jmcatRunners . _Just . folded
                . filteredBy (to _jrcSelectionId . only runnerId)
                . to _jrcRunnerName
    findRunnerName _ _ = Nothing

-- | Finds next race and subscribes to it
subsribeToNextGame :: Members '[Embed IO, StreamingHandler, BettingHandler, State AppState, Error String] r
                   => Sem r ()
subsribeToNextGame = do
    let eventTypeId = "7" -- Horse racing
    nextGame <- findNextRace eventTypeId
    case nextGame of
      Just market -> do
          forM_ (_jmcatMarketStartTime market) $ \t ->
              liftIO $ putStrLn $ "Next race: '" <> T.unpack (_jmcatMarketName market)
                                <> "', starting at: " <> show t

          let marketId = _jmcatMarketId market
          -- Subscribe to market stream
          subscribeToMarkets $ mkMarketListFilter [marketId]
          -- Update state
          put $ HasRaceInFuture (marketId, market)
      Nothing -> throw "Failed to find next game"

-- | Stream message processing loop
followHorseRacing :: Members '[Embed IO, StreamingHandler, BettingHandler, State AppState, Error String] r
                  => Sem r ()
followHorseRacing = forever $ do
    msg <- getNextStreamMessage
    case msg of
      SMConnectionStateChanged SMCSConnected -> do
          liftIO $ putStrLn "Connected, gathering initial data..."
          subsribeToNextGame

      -- Match non empty list. Empty list means 'heartbeat' message was received
      SMMarketUpdate marketIds@(_:_) ->
          getMarketCache >>= processMarketUpdate marketIds

      _ -> return ()

fetchLoginCredentials :: Members [Embed IO, Error String] r => Sem r LoginCredentials
fetchLoginCredentials =
    newLoginCredentials "John"				-- Username
                        "Pass123"			-- Password
                        "J7$q*cFQ[dxvCMJj"		-- App key
                        "public.pem"			-- Public certificate
                        "private.pem"			-- Private key
                        (fromInteger $ 4 * 60 * 60)	-- Token timeout - 4 hours

main :: IO ()
main = do
    credsResult <- runM . runError $ fetchLoginCredentials
    case credsResult of
      Left err -> print err
      Right creds -> do
        res <- runFinal
             . embedToFinal
             . asyncToIOFinal
             . resourceToIOFinal
             . errorToIOFinal
             . ignoreOutput
             . evalState Initial
             . runLoginHandler creds Nothing
             . runStreamingHandler defaultStreamingConnectionInfo
             . runBettingHandler defaultBettingUrl Nothing
             . runHeartbeatHandler defaultHeartbeatUrl Nothing 60
             $ followHorseRacing
        either print (const $ pure ()) res
```

## Documentation

See [here](https://raw.githack.com/martinserts/bf-haskell/master/docs/index.html)

