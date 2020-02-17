-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.BettingAPI.Betting
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module BfHaskell.BettingAPI.Betting
(
    BettingHandler(..)
  , runBettingHandler
  , defaultBettingUrl
  , createMarketFilter
  , listEventTypes, listTimeRanges, listCompetitions, listMarketCatalogue
  , listMarkets, listMarketBook
  , placeOrders
  , cancelOrders, cancelMarketOrders, cancelAllOrders
  , replaceOrders
) where

import           BfHaskell.BettingAPI.Request
import           BfHaskell.BettingAPI.Response
import           BfHaskell.BettingAPI.Types
import           BfHaskell.Common.Logging
import           BfHaskell.DSL.Login           (LoginHandler)
import           BfHaskell.Internal.Network    (parseUrl)
import           BfHaskell.Internal.Rpc        (performRpcRequest)
import           Data.Default
import           Data.Maybe                    (fromMaybe)
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import qualified Data.Vector                   as V
import           Network.HTTP.Req              (HttpConfig, Option,
                                                Scheme (Https), Url,
                                                defaultHttpConfig)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader

-- | Default login url
defaultBettingUrl :: Text
defaultBettingUrl = "https://api.betfair.com/exchange/betting/json-rpc/v1"

data BettingConfig = BettingConfig
        { _bcUrl     :: Url 'Https
        , _bcOptions :: Option 'Https
        }

-- | Betting DSL
data BettingHandler m a where
    ListEventTypes :: JsonRequestListEventTypes -> BettingHandler m JsonResponseListEventTypes
    ListTimeRanges :: JsonRequestListTimeRanges -> BettingHandler m JsonResponseListTimeRanges
    ListCompetitions :: JsonRequestListCompetitions -> BettingHandler m JsonResponseListCompetitions
    ListMarketCatalogue :: JsonRequestListMarketCatalogue -> BettingHandler m JsonResponseListMarketCatalogue
    ListMarkets :: JsonMarketFilter -> BettingHandler m JsonResponseListMarketCatalogue
    ListMarketBook :: JsonRequestListMarketBook -> BettingHandler m JsonResponseListMarketBook
    PlaceOrders :: JsonRequestPlaceOrders -> BettingHandler m JsonPlaceExecutionReport
    CancelOrders :: JsonRequestCancelOrders -> BettingHandler m JsonCancelExecutionReport
    CancelMarketOrders :: Text -> BettingHandler m JsonCancelExecutionReport
    CancelAllOrders :: BettingHandler m JsonCancelExecutionReport
    ReplaceOrders :: JsonRequestReplaceOrders -> BettingHandler m JsonReplaceExecutionReport

makeSem ''BettingHandler


-- | Helper to create market filter.
createMarketFilter :: Text                 -- ^ eventTypeId
                   -> Maybe UTCTime        -- ^ dateFrom
                   -> Maybe UTCTime        -- ^ dateTill
                   -> Maybe Text           -- ^ competitionId
                   -> JsonMarketFilter
createMarketFilter eid df dt cid =
    def { _jsmfEventTypeIds = Just $ V.singleton eid
    , _jsmfMarketStartTime = startTime
    , _jsmfCompetitionIds = competitionId
    }
  where
    startTime = case (df, dt) of
                  (Nothing, Nothing) -> Nothing
                  _                  -> Just $ JsonTimeRange df dt
    competitionId = fmap V.singleton cid

runBettingHandler :: Members '[Embed IO,
                               LoginHandler,
                               Output LogMessage,
                               Error String] r
                  => Text               -- ^ Betting API url
                  -> Maybe HttpConfig   -- ^ Override 'HttpConfig' if needed. Use 'Nothing' for default configuration.
                  -> InterpreterFor BettingHandler r
runBettingHandler url httpConfig sem = do
    let httpConfig' = fromMaybe defaultHttpConfig httpConfig
    bettingConfig <- uncurry BettingConfig <$> parseUrl url

    runReader httpConfig'
        $ reinterpret (\case
            ListEventTypes req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/listEventTypes" req
            ListTimeRanges req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/listTimeRanges" req
            ListCompetitions req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/listCompetitions" req
            ListMarketCatalogue req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/listMarketCatalogue" req
            ListMarkets marketFilter ->
                let mproj = S.fromList [COMPETITION, EVENT, EVENT_TYPE,
                                        MARKET_START_TIME, RUNNER_DESCRIPTION,
                                        MARKET_DESCRIPTION]
                    req = JsonRequestListMarketCatalogue marketFilter mproj FIRST_TO_START 200
                in bettingRequest bettingConfig "SportsAPING/v1.0/listMarketCatalogue" req
            ListMarketBook req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/listMarketBook" req
            PlaceOrders req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/placeOrders" req
            CancelOrders req ->
                cancelOrders' bettingConfig req
            CancelMarketOrders mid ->
                cancelOrders' bettingConfig $ JsonRequestCancelOrders (Just mid) V.empty def
            CancelAllOrders ->
                cancelOrders' bettingConfig $ JsonRequestCancelOrders def V.empty def

            ReplaceOrders req ->
                bettingRequest bettingConfig "SportsAPING/v1.0/replaceOrders" req
          ) sem
  where
    bettingRequest (BettingConfig url' opts) = performRpcRequest url' opts

    cancelOrders' config = bettingRequest config "SportsAPING/v1.0/cancelOrders"

