-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.CommandExecutor
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.CommandExecutor
(
    runCommandExecutor
) where

import           BfHaskell.Common.Logging
import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.Prices         (defaultLadderLevels)
import           BfHaskell.StreamingAPI.StreamingComm  (CommCentre, StreamingControlMessage (..),
                                                        getNextMessageId,
                                                        modifyMarketCache,
                                                        modifyOrderCache,
                                                        readConnection,
                                                        readControlMessage)
import           BfHaskell.StreamingAPI.StreamingUtils (sendStreamMessage)
import           BfHaskell.StreamingAPI.Types          (scSubscriptionId)
import qualified Control.Concurrent.Async              as AS
import           Control.Lens                          (set)
import           Control.Monad.IO.Class                (liftIO)
import qualified Data.Text                             as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

runCommandExecutor :: Members '[Embed IO,
                               Output LogMessage,
                               Error String] r
                   => AS.Async a
                   -> CommCentre
                   -> Sem r ()
runCommandExecutor sslClientThread comm = do
    logInfo "runCommandExecutor - starting"
    go
    logInfo "runCommandExecutor - finished"
  where
    go = do
        cmd <- liftIO $ readControlMessage comm
        logInfo $ "runCommandExecutor - incoming command: " <>
                        T.pack (show cmd)
        case cmd of
          SCMSubscribeToMarkets markets -> subscribeToMarkets markets comm >> go
          SCMSubscribeToOrders          -> subscribeToOrders comm >> go
          SCMStopService                -> liftIO $ AS.cancel sslClientThread

subscribeToMarkets :: Members '[Embed IO,
                                Output LogMessage,
                                Error String] r
                   => MarketFilter
                   -> CommCentre
                   -> Sem r ()
subscribeToMarkets marketFilter comm = do
    mConn <- liftIO $ readConnection comm
    case mConn of
      Nothing -> throw "Tried to subscribe to markets when disconnected"
      Just conn -> do
          -- Get next unique message id
          msgId <- liftIO $ getNextMessageId comm

          -- Update market change subscription id with newly generated id
          modifyMarketCache comm $ \mc -> (set scSubscriptionId msgId mc, ())

          -- Send market subscription message
          let msg = mkMarketSubscriptionMessage
                    { marketSubscriptionMessageId = Just msgId
                    , marketSubscriptionMessageSegmentationEnabled = Just True
                    , marketSubscriptionMessageClk = Nothing -- TODO: use clk?
                    , marketSubscriptionMessageInitialClk = Nothing  -- TODO: use clk?
                    , marketSubscriptionMessageMarketDataFilter = dataFilter
                    , marketSubscriptionMessageMarketFilter = Just marketFilter
                    }
          sendStreamMessage conn msg
  where
    dataFilter = Just $ mkMarketDataFilter
        { marketDataFilterLadderLevels = Just defaultLadderLevels
        , marketDataFilterFields = Just [E'Fields'EX_BEST_OFFERS,
                                         E'Fields'EX_MARKET_DEF]
        }

subscribeToOrders :: Members '[Embed IO,
                               Output LogMessage,
                               Error String] r
                   => CommCentre
                   -> Sem r ()
subscribeToOrders comm = do
    mConn <- liftIO $ readConnection comm
    case mConn of
      Nothing -> throw "Tried to subscribe to orders when disconnected"
      Just conn -> do
          -- Get next unique message id
          msgId <- liftIO $ getNextMessageId comm

          -- Update order change subscription id with newly generated id
          modifyOrderCache comm $ \mc -> (set scSubscriptionId msgId mc, ())

          -- Send order subscription message
          let msg = mkOrderSubscriptionMessage
                    { orderSubscriptionMessageId = Just msgId
                    , orderSubscriptionMessageSegmentationEnabled = Just True
                    , orderSubscriptionMessageClk = Nothing -- TODO: use clk?
                    , orderSubscriptionMessageInitialClk = Nothing -- TODO: use clk?
                    }
          sendStreamMessage conn msg
