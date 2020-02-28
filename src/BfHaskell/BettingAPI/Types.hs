-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.BettingAPI.Types
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module BfHaskell.BettingAPI.Types
(
    JsonMarketBettingType(..)
  , JsonTimeRange(..)
  , JsonOrderStatus(..)
  , JsonMarketFilter(..), jsmfTextQuery, jsmfExchangeIds, jsmfEventTypeIds
  , jsmfEventIds, jsmfCompetitionIds, jsmfMarketIds, jsmfVenues, jsmfBspOnly
  , jsmfTurnInPlayEnabled, jsmfInPlayOnly, jsmfMarketBettingTypes
  , jsmfMarketCountries, jsmfMarketTypeCodes, jsmfMarketStartTime
  , jsmfWithOrders, jsmfRaceTypes
  , JsonTimeGranularity(..)
  , JsonMarketProjection(..)
  , JsonMarketSort(..)
  , JsonEventType(..)
  , JsonCompetition(..)
  , JsonPriceLadderType(..)
  , JsonPriceLadderDescription(..)
  , JsonMarketLineRangeInfo(..)
  , JsonMarketDescription(..)
  , JsonRunnerCatalog(..)
  , JsonEvent(..)
  , JsonMarketCatalogue(..)
  , JsonPriceData(..)
  , JsonRollupModel(..)
  , JsonExBestOffersOverrides(..)
  , JsonPriceProjection(..)
  , JsonOrderProjection(..)
  , JsonMatchProjection(..)
  , JsonMarketStatus(..)
  , JsonRunnerStatus(..)
  , JsonPriceSize(..)
  , JsonStartingPrices(..)
  , JsonExchangePrices(..)
  , JsonOrderType(..)
  , JsonPersistenceType(..)
  , JsonSide(..)
  , JsonOrder(..)
  , JsonMatch(..)
  , JsonRunner(..)
  , JsonMarketBook(..)
  , JsonTimeInForce(..)
  , JsonBetTargetType(..)
  , JsonLimitOrder(..)
  , JsonLimitOnCloseOrder(..)
  , JsonMarketOnCloseOrder(..)
  , JsonPlaceInstruction(..)
  , JsonMarketVersion(..)
  , JsonExecutionReportStatus(..)
  , JsonExecutionReportErrorCode(..)
  , JsonInstructionReportStatus(..)
  , JsonInstructionReportErrorCode(..)
  , JsonPlaceInstructionReport(..)
  , JsonPlaceExecutionReport(..)
  , JsonCancelInstruction(..)
  , JsonCancelInstructionReport(..)
  , JsonCancelExecutionReport(..)
  , JsonReplaceInstruction(..)
  , JsonReplaceInstructionReport(..)
  , JsonReplaceExecutionReport(..)
) where

import           BfHaskell.Internal.JsonTypes (defaultEnumFromJsonOptions,
                                               defaultEnumToJsonOptions,
                                               defaultFromJsonOptions,
                                               defaultToJsonOptions)
import           Control.Lens                 (makeLenses)
import qualified Data.Aeson                   as A
import           Data.Default
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import qualified Data.Vector                  as V
import           GHC.Generics                 (Generic)


data JsonMarketBettingType = ODDS | LINE | RANGE | ASIAN_HANDICAP_DOUBLE_LINE
                                  | ASIAN_HANDICAP_SINGLE_LINE | FIXED_ODDS
                             deriving (Eq, Show, Generic)
instance A.ToJSON JsonMarketBettingType
instance A.FromJSON JsonMarketBettingType

data JsonTimeRange = JsonTimeRange { _jstrFrom :: Maybe UTCTime
                                   , _jstrTo   :: Maybe UTCTime
                                   } deriving (Show, Generic)
instance A.ToJSON JsonTimeRange where
    toJSON = A.genericToJSON defaultToJsonOptions
instance A.FromJSON JsonTimeRange

data JsonOrderStatus = PENDING | EXECUTION_COMPLETE | EXECUTABLE
                       | EXPIRED deriving (Eq, Show, Generic)
instance A.ToJSON JsonOrderStatus
instance A.FromJSON JsonOrderStatus

data JsonMarketFilter = JsonMarketFilter
        { _jsmfTextQuery          :: Maybe Text
        , _jsmfExchangeIds        :: Maybe (V.Vector Text)
        , _jsmfEventTypeIds       :: Maybe (V.Vector Text)
        , _jsmfEventIds           :: Maybe (V.Vector Text)
        , _jsmfCompetitionIds     :: Maybe (V.Vector Text)
        , _jsmfMarketIds          :: Maybe (V.Vector Text)
        , _jsmfVenues             :: Maybe (V.Vector Text)
        , _jsmfBspOnly            :: Maybe Bool
        , _jsmfTurnInPlayEnabled  :: Maybe Bool
        , _jsmfInPlayOnly         :: Maybe Bool
        , _jsmfMarketBettingTypes :: Maybe (V.Vector JsonMarketBettingType)
        , _jsmfMarketCountries    :: Maybe (V.Vector Text)
        , _jsmfMarketTypeCodes    :: Maybe (V.Vector Text)
        , _jsmfMarketStartTime    :: Maybe JsonTimeRange
        , _jsmfWithOrders         :: Maybe (V.Vector JsonOrderStatus)
        , _jsmfRaceTypes          :: Maybe (V.Vector Text)
        } deriving (Show, Generic)
instance A.ToJSON JsonMarketFilter where
    toJSON = A.genericToJSON defaultToJsonOptions
makeLenses ''JsonMarketFilter

instance Default JsonMarketFilter where
    def = JsonMarketFilter Nothing Nothing Nothing Nothing Nothing Nothing
                           Nothing Nothing Nothing Nothing Nothing Nothing
                           Nothing Nothing Nothing Nothing

data JsonTimeGranularity = DAYS | HOURS | MINUTES
                           deriving (Eq, Show, Generic)
instance A.ToJSON JsonTimeGranularity

data JsonMarketProjection = COMPETITION | EVENT | EVENT_TYPE | MARKET_START_TIME
                            | MARKET_DESCRIPTION | RUNNER_DESCRIPTION
                            | RUNNER_METADATA deriving (Show, Generic, Eq, Ord)
instance A.ToJSON JsonMarketProjection

data JsonMarketSort = MINIMUM_TRADED | MAXIMUM_TRADED | MINIMUM_AVAILABLE
                      | MAXIMUM_AVAILABLE | FIRST_TO_START | LAST_TO_START
                      deriving (Eq, Show, Generic)
instance A.ToJSON JsonMarketSort

data JsonEventType = JsonEventType { _jsetId   :: Text
                                   , _jsetName :: Text
                                   } deriving (Show, Generic)
instance A.FromJSON JsonEventType where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonEventType where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonCompetition = JsonCompetition
        { _jpcpId   :: Text
        , _jpcpName :: Text
        } deriving (Show, Generic)
instance A.FromJSON JsonCompetition where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonCompetition where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonPriceLadderType = CLASSIC | FINEST | LINE_RANGE
                           deriving (Eq, Show, Generic)
instance A.FromJSON JsonPriceLadderType
instance A.ToJSON JsonPriceLadderType

newtype JsonPriceLadderDescription = JsonPriceLadderDescription
        { _jpldType :: JsonPriceLadderType } deriving (Show, Generic)
instance A.FromJSON JsonPriceLadderDescription where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonPriceLadderDescription where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonMarketLineRangeInfo = JsonMarketLineRangeInfo
        { _jmlriMaxUnitValue :: Double
        , _jmlriMinUnitValue :: Double
        , _jmlriInterval     :: Double
        , _jmlriMarketUnit   :: Text
        } deriving (Show, Generic)
instance A.FromJSON JsonMarketLineRangeInfo where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonMarketLineRangeInfo where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonMarketDescription = JsonMarketDescription
        { _jmdPersistenceEnabled     :: Bool
        , _jmdBspMarket              :: Bool
        , _jmdMarketTime             :: UTCTime
        , _jmdSuspendTime            :: UTCTime
        , _jmdSettleTime             :: Maybe UTCTime
        , _jmdBettingType            :: JsonMarketBettingType
        , _jmdTurnInPlayEnabled      :: Bool
        , _jmdMarketType             :: Text
        , _jmdRegulator              :: Text
        , _jmdMarketBaseRate         :: Double
        , _jmdDiscountAllowed        :: Bool
        , _jmdWallet                 :: Maybe Text
        , _jmdRules                  :: Maybe Text
        , _jmdRulesHasDate           :: Maybe Bool
        , _jmdEachWayDivisor         :: Maybe Double
        , _jmdClarifications         :: Maybe Text
        , _jmdLineRangeInfo          :: Maybe JsonMarketLineRangeInfo
        , _jmdRaceType               :: Maybe Text
        , _jmdPriceLadderDescription :: Maybe JsonPriceLadderDescription
        } deriving (Show, Generic)
instance A.FromJSON JsonMarketDescription where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonMarketDescription where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRunnerCatalog = JsonRunnerCatalog
        { _jrcSelectionId  :: Integer
        , _jrcRunnerName   :: Text
        , _jrcHandicap     :: Double
        , _jrcSortPriority :: Int
        } deriving (Show, Generic)
instance A.FromJSON JsonRunnerCatalog where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonRunnerCatalog where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonEvent = JsonEvent
        { _jevtId          :: Maybe Text
        , _jevtName        :: Maybe Text
        , _jevtCountryCode :: Maybe Text
        , _jevtTimezone    :: Maybe Text
        , _jevtVenue       :: Maybe Text
        , _jevtOpenDate    :: Maybe UTCTime
        } deriving (Show,Generic)
instance A.FromJSON JsonEvent where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonEvent where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonMarketCatalogue = JsonMarketCatalogue
        { _jmcatMarketId        :: Text
        , _jmcatMarketName      :: Text
        , _jmcatMarketStartTime :: Maybe UTCTime
        , _jmcatDescription     :: Maybe JsonMarketDescription
        , _jmcatTotalMatched    :: Maybe Double
        , _jmcatRunners         :: Maybe (V.Vector JsonRunnerCatalog)
        , _jmcatEventType       :: Maybe JsonEventType
        , _jmcatCompetition     :: Maybe JsonCompetition
        , _jmcatEvent           :: Maybe JsonEvent
        } deriving (Show, Generic)
instance A.FromJSON JsonMarketCatalogue where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonMarketCatalogue where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonPriceData = SP_AVAILABLE | SP_TRADED | EX_BEST_OFFERS | EX_ALL_OFFERS
                     | EX_TRADED deriving (Eq, Show, Generic)
instance A.ToJSON JsonPriceData

data JsonRollupModel = STAKE | PAYOUT | MANAGED_LIABILITY | NONE
                       deriving (Eq, Show, Generic)
instance A.ToJSON JsonRollupModel

data JsonExBestOffersOverrides = JsonExBestOffersOverrides
        { _jebooBestPricesDepth          :: Maybe Int
        , _jebooRollupModel              :: Maybe JsonRollupModel
        , _jebooRollupLimit              :: Maybe Int
        , _jebooRollupLiabilityThreshold :: Maybe Double
        , _jebooRollupLiabilityFactor    :: Maybe Int
        } deriving (Show, Generic)
instance A.ToJSON JsonExBestOffersOverrides where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonPriceProjection = JsonPriceProjection
        { _jppPriceData             :: S.Set JsonPriceData
        , _jppExBestOffersOverrides :: Maybe JsonExBestOffersOverrides
        , _jppVirtualise            :: Maybe Bool
        , _jppRolloverStakes        :: Maybe Bool
        } deriving (Show, Generic)
instance A.ToJSON JsonPriceProjection where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonOrderProjection = JOP_ALL | JOP_EXECUTABLE | JOP_EXECUTION_COMPLETE
                           deriving (Eq, Show, Generic)
instance A.ToJSON JsonOrderProjection where
    toJSON = A.genericToJSON defaultEnumToJsonOptions

data JsonMatchProjection = NO_ROLLUP | ROLLED_UP_BY_PRICE
                           | ROLLED_UP_BY_AVG_PRICE deriving (Eq, Show, Generic)
instance A.ToJSON JsonMatchProjection

data JsonMarketStatus = INACTIVE | OPEN | SUSPENDED | CLOSED
                        deriving (Eq, Show, Generic)
instance A.FromJSON JsonMarketStatus

data JsonRunnerStatus = ACTIVE | WINNER | LOSER | PLACED | REMOVED_VACANT
                        | REMOVED | HIDDEN deriving (Eq, Show, Generic)
instance A.FromJSON JsonRunnerStatus

data JsonPriceSize = JsonPriceSize
        { _jpszPrice :: Double
        , _jpszSize  :: Double
        } deriving (Show, Generic)
instance A.FromJSON JsonPriceSize where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonStartingPrices = JsonStartingPrices
        { _jsprNearPrice         :: Maybe Double
        , _jsprFarPrice          :: Maybe Double
        , _jsprBackStakeTaken    :: Maybe (V.Vector JsonPriceSize)
        , _jsprLayLiabilityTaken :: Maybe (V.Vector JsonPriceSize)
        , _jsprActualSP          :: Maybe Double
        } deriving (Show, Generic)
instance A.FromJSON JsonStartingPrices where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonExchangePrices = JsonExchangePrices
        { _jeprAvailableToBack :: Maybe (V.Vector JsonPriceSize)
        , _jeprAvailableToLay  :: Maybe (V.Vector JsonPriceSize)
        , _jeprTradedVolume    :: Maybe (V.Vector JsonPriceSize)
        } deriving (Show, Generic)
instance A.FromJSON JsonExchangePrices where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonOrderType = LIMIT | LIMIT_ON_CLOSE | MARKET_ON_CLOSE
                     deriving (Eq, Show, Generic)
instance A.FromJSON JsonOrderType
instance A.ToJSON JsonOrderType

data JsonPersistenceType = JPT_LAPSE | JPT_PERSIST | JPT_MARKET_ON_CLOSE
                           deriving (Eq, Show, Generic)
instance A.FromJSON JsonPersistenceType  where
     parseJSON = A.genericParseJSON defaultEnumFromJsonOptions
instance A.ToJSON JsonPersistenceType where
    toJSON = A.genericToJSON defaultEnumToJsonOptions

data JsonSide = BACK | LAY deriving (Eq, Show, Generic)
instance A.FromJSON JsonSide
instance A.ToJSON JsonSide

data JsonOrder = JsonOrder
        { _jsorBetId               :: Text
        , _jsorOrderType           :: JsonOrderType
        , _jsorStatus              :: JsonOrderStatus
        , _jsorPersistenceType     :: JsonPersistenceType
        , _jsorSide                :: JsonSide
        , _jsorPrice               :: Double
        , _jsorSize                :: Double
        , _jsorBspLiability        :: Double
        , _jsorPlacedDate          :: UTCTime
        , _jsorAvgPriceMatched     :: Maybe Double
        , _jsorSizeMatched         :: Maybe Double
        , _jsorSizeRemaining       :: Maybe Double
        , _jsorSizeLapsed          :: Maybe Double
        , _jsorSizeCancelled       :: Maybe Double
        , _jsorSizeVoided          :: Maybe Double
        , _jsrCustomerOrderRef     :: Maybe Text
        , _jsorCustomerStrategyRef :: Maybe Text
        } deriving (Show, Generic)
instance A.FromJSON JsonOrder where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonMatch = JsonMatch
        { _jsmaBetId     :: Maybe Text
        , _jsmaMatchId   :: Maybe Text
        , _jsmaSide      :: JsonSide
        , _jsmaPrice     :: Double
        , _jsmaSize      :: Double
        , _jsmaMatchDate :: UTCTime
        } deriving (Show, Generic)
instance A.FromJSON JsonMatch where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonRunner = JsonRunner
        { _jsrnSelectionId      :: Integer
        , _jsrnHandicap         :: Double
        , _jsrnStatus           :: JsonRunnerStatus
        , _jsrnAdjustmentFactor :: Double
        , _jsrnLastPriceTraded  :: Maybe Double
        , _jsrnTotalMatched     :: Maybe Double
        , _jsrnRemovalDate      :: Maybe UTCTime
        , _jsrnSp               :: Maybe JsonStartingPrices
        , _jsrnEx               :: Maybe JsonExchangePrices
        , _jsrnOrders           :: Maybe (V.Vector JsonOrder)
        , _jsrnMatched          :: Maybe (V.Vector JsonMatch)
        } deriving (Show, Generic)
instance A.FromJSON JsonRunner where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonMarketBook = JsonMarketBook
        { _jsmbMarketId              :: Text
        , _jsmbIsMarketDataDelayed   :: Bool
        , _jsmbStatus                :: Maybe JsonMarketStatus
        , _jsmbBetDelay              :: Maybe Int
        , _jsmbBspReconciled         :: Maybe Bool
        , _jsmbComplete              :: Maybe Bool
        , _jsmbInplay                :: Maybe Bool
        , _jsmbNumberOfWinners       :: Maybe Int
        , _jsmbNumberOfRunners       :: Maybe Int
        , _jsmbNumberOfActiveRunners :: Maybe Int
        , _jsmbLastMatchTime         :: Maybe UTCTime
        , _jsmbTotalMatched          :: Maybe Double
        , _jsmbTotalAvailable        :: Maybe Double
        , _jsmbCrossMatching         :: Maybe Bool
        , _jsmbRunnersVoidable       :: Maybe Bool
        , _jsmbVersion               :: Maybe Integer
        , _jsmbRunners               :: Maybe (V.Vector JsonRunner)
        } deriving (Show, Generic)
instance A.FromJSON JsonMarketBook where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonTimeInForce = FILL_OR_KILL deriving (Eq, Show, Generic)
instance A.ToJSON JsonTimeInForce
instance A.FromJSON JsonTimeInForce

data JsonBetTargetType = JBTT_BACKERS_PROFIT | JBTT_PAYOUT
                         deriving (Eq, Show, Generic)
instance A.ToJSON JsonBetTargetType where
    toJSON = A.genericToJSON defaultEnumToJsonOptions
instance A.FromJSON JsonBetTargetType where
     parseJSON = A.genericParseJSON defaultEnumFromJsonOptions

data JsonLimitOrder = JsonLimitOrder
        { _jsloSize            :: Double
        , _jsloPrice           :: Double
        , _jsloPersistenceType :: JsonPersistenceType
        , _jsloTimeInForce     :: Maybe JsonTimeInForce
        , _jsloMinFillSize     :: Maybe Double
        , _jsloBetTargetType   :: Maybe JsonBetTargetType
        , _jsloBetTargetSize   :: Maybe Double
        } deriving (Show, Generic)
instance A.ToJSON JsonLimitOrder where
    toJSON = A.genericToJSON defaultToJsonOptions
instance A.FromJSON JsonLimitOrder where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonLimitOnCloseOrder = JsonLimitOnCloseOrder
        { _jlocoLiability :: Double
        , _jlocoPrice     :: Double
        } deriving (Show, Generic)
instance A.ToJSON JsonLimitOnCloseOrder where
    toJSON = A.genericToJSON defaultToJsonOptions
instance A.FromJSON JsonLimitOnCloseOrder where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

newtype JsonMarketOnCloseOrder = JsonMarketOnCloseOrder
        { _jmocoLiability :: Double } deriving (Show, Generic)
instance A.ToJSON JsonMarketOnCloseOrder where
    toJSON = A.genericToJSON defaultToJsonOptions
instance A.FromJSON JsonMarketOnCloseOrder where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonPlaceInstruction = JsonPlaceInstruction
        { _jpliOrderType          :: JsonOrderType
        , _jpliSelectionId        :: Integer
        , _jpliHandicap           :: Maybe Double
        , _jpliSide               :: JsonSide
        , _jpliLimitOrder         :: Maybe JsonLimitOrder
        , _jpliLimitOnCloseOrder  :: Maybe JsonLimitOnCloseOrder
        , _jpliMarketOnCloseOrder :: Maybe JsonMarketOnCloseOrder
        , _jpliCustomerOrderRef   :: Maybe Text
        } deriving (Show, Generic)
instance A.ToJSON JsonPlaceInstruction where
    toJSON = A.genericToJSON defaultToJsonOptions
instance A.FromJSON JsonPlaceInstruction where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

newtype JsonMarketVersion = JsonMarketVersion
        { _jmverVersion :: Maybe Int } deriving (Show, Generic)
instance A.ToJSON JsonMarketVersion where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonExecutionReportStatus = JERS_SUCCESS | JERS_FAILURE
                                 | JERS_PROCESSED_WITH_ERRORS | TIMEOUT
                                 deriving (Eq, Show, Generic)
instance A.FromJSON JsonExecutionReportStatus where
     parseJSON = A.genericParseJSON defaultEnumFromJsonOptions

data JsonExecutionReportErrorCode =
        ERROR_IN_MATCHER | PROCESSED_WITH_ERRORS | BET_ACTION_ERROR
        | INVALID_ACCOUNT_STATE | INVALID_WALLET_STATUS | INSUFFICIENT_FUNDS
        | LOSS_LIMIT_EXCEEDED | MARKET_SUSPENDED | MARKET_NOT_OPEN_FOR_BETTING
        | DUPLICATE_TRANSACTION | INVALID_ORDER | INVALID_MARKET_ID
        | PERMISSION_DENIED | DUPLICATE_BETIDS | NO_ACTION_REQUIRED
        | SERVICE_UNAVAILABLE | REJECTED_BY_REGULATOR | NO_CHASING
        | REGULATOR_IS_NOT_AVAILABLE | TOO_MANY_INSTRUCTIONS
        | INVALID_MARKET_VERSION deriving (Eq, Show, Generic)
instance A.FromJSON JsonExecutionReportErrorCode

data JsonInstructionReportStatus = JIRS_SUCCESS | JIRS_FAILURE | JIRS_TIMEOUT
                                   deriving (Eq, Show, Generic)
instance A.FromJSON JsonInstructionReportStatus where
     parseJSON = A.genericParseJSON defaultEnumFromJsonOptions

data JsonInstructionReportErrorCode =
        JREC_INVALID_BET_SIZE | JREC_INVALID_RUNNER | JREC_BET_TAKEN_OR_LAPSED
        | JREC_BET_IN_PROGRESS | JREC_RUNNER_REMOVED | JREC_MARKET_NOT_OPEN_FOR_BETTING
        | JREC_LOSS_LIMIT_EXCEEDED | JREC_MARKET_NOT_OPEN_FOR_BSP_BETTING
        | JREC_INVALID_PRICE_EDIT | JREC_INVALID_ODDS | JREC_INSUFFICIENT_FUNDS
        | JREC_INVALID_PERSISTENCE_TYPE | JREC_ERROR_IN_MATCHER
        | JREC_INVALID_BACK_LAY_COMBINATION | JREC_ERROR_IN_ORDER
        | JREC_INVALID_BID_TYPE | JREC_INVALID_BET_ID | JREC_CANCELLED_NOT_PLACED
        | JREC_RELATED_ACTION_FAILED | JREC_NO_ACTION_REQUIRED
        | JREC_TIME_IN_FORCE_CONFLICT | JREC_UNEXPECTED_PERSISTENCE_TYPE
        | JREC_INVALID_ORDER_TYPE | JREC_UNEXPECTED_MIN_FILL_SIZE
        | JREC_INVALID_CUSTOMER_ORDER_REF | JREC_INVALID_MIN_FILL_SIZE
        deriving (Eq, Show, Generic)
instance A.FromJSON JsonInstructionReportErrorCode where
     parseJSON = A.genericParseJSON defaultEnumFromJsonOptions

data JsonPlaceInstructionReport = JsonPlaceInstructionReport
        { _jpirStatus              :: JsonInstructionReportStatus
        , _jpirErrorCode           :: Maybe JsonInstructionReportErrorCode
        , _jpirOrderStatus         :: Maybe JsonOrderStatus
        , _jpirInstruction         :: JsonPlaceInstruction
        , _jpirBetId               :: Maybe Text
        , _jpirPlacedDate          :: Maybe UTCTime
        , _jpirAveragePriceMatched :: Maybe Double
        , _jpirSizeMatched         :: Maybe Double
        } deriving (Show, Generic)
instance A.FromJSON JsonPlaceInstructionReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonPlaceExecutionReport = JsonPlaceExecutionReport
        { _jperCustomerRef        :: Maybe Text
        , _jperStatus             :: JsonExecutionReportStatus
        , _jperErrorCode          :: Maybe JsonExecutionReportErrorCode
        , _jperMarketId           :: Maybe Text
        , _jperInstructionReports :: Maybe (V.Vector JsonPlaceInstructionReport)
        } deriving (Show, Generic)
instance A.FromJSON JsonPlaceExecutionReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonCancelInstruction = JsonCancelInstruction
        { _jciBetId         :: Text
        , _jciSizeReduction :: Maybe Double
        } deriving (Show, Generic)
instance A.ToJSON JsonCancelInstruction where
    toJSON = A.genericToJSON defaultToJsonOptions
instance A.FromJSON JsonCancelInstruction where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonCancelInstructionReport = JsonCancelInstructionReport
        { _jcirStatus        :: JsonInstructionReportStatus
        , _jcirErrorCode     :: Maybe JsonInstructionReportErrorCode
        , _jcirInstruction   :: Maybe JsonCancelInstruction
        , _jcirSizeCancelled :: Maybe Double
        , _jcirCancelledDate :: Maybe UTCTime
        } deriving (Show, Generic)
instance A.FromJSON JsonCancelInstructionReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonCancelExecutionReport = JsonCancelExecutionReport
        { _jcerCustomerRef :: Maybe Text
        , _jcerStatus :: JsonExecutionReportStatus
        , _jcerErrorCode :: Maybe JsonExecutionReportErrorCode
        , _jcerMarketId :: Maybe Text
        , _jcerInstructionReports :: Maybe (V.Vector JsonCancelInstructionReport)
        } deriving (Show, Generic)
instance A.FromJSON JsonCancelExecutionReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonReplaceInstruction = JsonReplaceInstruction
        { _jrpiBetId    :: Text
        , _jrpiNewPrice :: Double
        } deriving (Show, Generic)
instance A.ToJSON JsonReplaceInstruction where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonReplaceInstructionReport = JsonReplaceInstructionReport
        { _jrirStatus                  :: JsonInstructionReportStatus
        , _jrirErrorCode               :: Maybe JsonInstructionReportErrorCode
        , _jrirCancelInstructionReport :: Maybe JsonCancelInstructionReport
        , _jrirPlaceInstructionReport  :: Maybe JsonPlaceInstructionReport
        } deriving (Show, Generic)
instance A.FromJSON JsonReplaceInstructionReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

data JsonReplaceExecutionReport = JsonReplaceExecutionReport
        { _jrerCustomerRef :: Maybe Text
        , _jrerStatus :: JsonExecutionReportStatus
        , _jrerErrorCode :: Maybe JsonExecutionReportErrorCode
        , _jrerMarketId :: Maybe Text
        , _jrerInstructionReports :: Maybe (V.Vector JsonReplaceInstructionReport)
        } deriving (Show, Generic)
instance A.FromJSON JsonReplaceExecutionReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

