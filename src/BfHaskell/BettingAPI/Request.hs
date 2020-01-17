-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.BettingAPI.Request
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE DeriveGeneric #-}

module BfHaskell.BettingAPI.Request
(
  JsonRequestListEventTypes(..)
  , JsonRequestListTimeRanges(..)
  , JsonRequestListCompetitions(..)
  , JsonRequestListMarketCatalogue(..)
  , JsonRequestListMarketBook(..)
  , JsonRequestPlaceOrders(..)
  , JsonRequestCancelOrders(..)
  , JsonRequestReplaceOrders(..)
) where

import           BfHaskell.BettingAPI.Types
import           BfHaskell.Internal.JsonTypes (defaultToJsonOptions)
import qualified Data.Aeson                   as A
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import qualified Data.Vector                  as V
import           GHC.Generics                 (Generic)


data JsonRequestListEventTypes = JsonRequestListEventTypes
        { _jrletFilter :: JsonMarketFilter
        , _jrletLocale :: Maybe Text
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestListEventTypes where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRequestListTimeRanges = JsonRequestListTimeRanges
        { _jrltrFilter      :: JsonMarketFilter
        , _jrltrGranularity :: JsonTimeGranularity
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestListTimeRanges where
    toJSON = A.genericToJSON defaultToJsonOptions

newtype JsonRequestListCompetitions = JsonRequestListCompetitions
        { _jrlcFilter :: JsonMarketFilter } deriving (Show, Generic)
instance A.ToJSON JsonRequestListCompetitions where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRequestListMarketCatalogue = JsonRequestListMarketCatalogue
        { _jrlmcFilter           :: JsonMarketFilter
        , _jrlmcMarketProjection :: S.Set JsonMarketProjection
        , _jrlmcSort             :: JsonMarketSort
        , _jrlmcMaxResults       :: Int
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestListMarketCatalogue where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRequestListMarketBook = JsonRequestListMarketBook
        { _jrlmbMarketIds                     :: V.Vector Text
        , _jlrmbPriceProjection               :: Maybe JsonPriceProjection
        , _jlrmbOrderProjection               :: Maybe JsonOrderProjection
        , _jlrmbMatchProjection               :: Maybe JsonMatchProjection
        , _jlrmbIncludeOverallPosition        :: Maybe Bool
        , _jlrmbPartitionMatchedByStrategyRef :: Maybe Bool
        , _jlrmbCustomerStrategyRefs          :: Maybe (S.Set Text)
        , _jlrmbCurrencyCode                  :: Maybe Text
        , _jlrmbLocale                        :: Maybe Text
        , _jlrmbMatchedSince                  :: Maybe UTCTime
        , _jlrmbBetIds                        :: Maybe (S.Set Text)
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestListMarketBook where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRequestPlaceOrders = JsonRequestPlaceOrders
        { _jrpoMarketId            :: Text
        , _jrpoInstructions        :: V.Vector JsonPlaceInstruction
        , _jrpoCustomerRef         :: Maybe Text
        , _jrpoMarketVersion       :: Maybe JsonMarketVersion
        , _jrpoCustomerStrategyRef :: Maybe Text
        , _jrpoAsync               :: Maybe Bool
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestPlaceOrders where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRequestCancelOrders = JsonRequestCancelOrders
        { _jrcoMarketId     :: Maybe Text
        , _jrcoInstructions :: V.Vector JsonCancelInstruction
        , _jrcoCustomerRef  :: Maybe Text
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestCancelOrders where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonRequestReplaceOrders = JsonRequestReplaceOrders
        { _jrroMarketId      :: Text
        , _jrroInstructions  :: V.Vector JsonReplaceInstruction
        , _jrroCustomerRef   :: Maybe Text
        , _jrroMarketVersion :: Maybe JsonMarketVersion
        , _jrroAsync         :: Maybe Bool
        } deriving (Show, Generic)
instance A.ToJSON JsonRequestReplaceOrders where
    toJSON = A.genericToJSON defaultToJsonOptions

