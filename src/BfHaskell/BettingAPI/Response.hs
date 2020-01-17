-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.BettingAPI.Response
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE DeriveGeneric #-}

module BfHaskell.BettingAPI.Response
(
  JsonEventTypeResult(..)
  , JsonResponseListEventTypes(..)
  , JsonResponseListTimeRanges(..)
  , JsonCompetitionResult(..)
  , JsonResponseListCompetitions(..)
  , JsonResponseListMarketCatalogue(..)
  , JsonResponseListMarketBook(..)
) where

import           BfHaskell.BettingAPI.Types
import           BfHaskell.Internal.JsonTypes (defaultFromJsonOptions,
                                               defaultToJsonOptions)
import qualified Data.Aeson                   as A
import qualified Data.Vector                  as V
import           GHC.Generics                 (Generic)

data JsonEventTypeResult = JsonEventTypeResult { _jsetrEventType :: JsonEventType
                                               , _jsetrMarketCount :: Int
                                               } deriving (Show, Generic)
instance A.FromJSON JsonEventTypeResult where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

newtype JsonResponseListEventTypes = JsonResponseListEventTypes
                                        (V.Vector JsonEventTypeResult)
                                        deriving (Show, Generic)
instance A.FromJSON JsonResponseListEventTypes

data JsonTimeRangeResult = JsonTimeRangeResult
        { _jpltrTimeRange   :: JsonTimeRange
        , _jpltrMarketCount :: Int
        } deriving (Show, Generic)
instance A.FromJSON JsonTimeRangeResult where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

newtype JsonResponseListTimeRanges = JsonResponseListTimeRanges
                                    (V.Vector JsonTimeRangeResult)
                                    deriving (Show, Generic)
instance A.FromJSON JsonResponseListTimeRanges

data JsonCompetitionResult = JsonCompetitionResult
        { _jpcprCompetition :: JsonCompetition
        , _jpcprMarketCount :: Int
        } deriving (Show, Generic)
instance A.FromJSON JsonCompetitionResult where
     parseJSON = A.genericParseJSON defaultFromJsonOptions

newtype JsonResponseListCompetitions = JsonResponseListCompetitions
                                        (V.Vector JsonCompetitionResult)
                                        deriving (Show, Generic)
instance A.FromJSON JsonResponseListCompetitions

newtype JsonResponseListMarketCatalogue = JsonResponseListMarketCatalogue
            (V.Vector JsonMarketCatalogue)
            deriving (Show, Generic)
instance A.FromJSON JsonResponseListMarketCatalogue
instance A.ToJSON JsonResponseListMarketCatalogue where
    toJSON = A.genericToJSON defaultToJsonOptions

newtype JsonResponseListMarketBook = JsonResponseListMarketBook
            (V.Vector JsonMarketBook)
            deriving (Show, Generic)

instance A.FromJSON JsonResponseListMarketBook

