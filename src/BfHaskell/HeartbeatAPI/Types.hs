-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.HeartbeatAPI.Types
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module BfHaskell.HeartbeatAPI.Types
(
    HeartbeatHandler(..)
  , getHeartbeatTimeout
  , JsonRequestHeartbeat(..)
  , JsonActionPerformed(..)
  , JsonHeartbeatReport(..)
  , defaultHeartbeatUrl
) where

import           BfHaskell.Internal.JsonTypes (defaultEnumFromJsonOptions,
                                               defaultFromJsonOptions,
                                               defaultToJsonOptions)
import qualified Data.Aeson                   as A
import           Data.Text                    (Text)
import           Data.Time.Clock              (NominalDiffTime)
import           GHC.Generics                 (Generic)
import           Polysemy


-- | Default login url
defaultHeartbeatUrl :: Text
defaultHeartbeatUrl = "https://api.betfair.com/exchange/heartbeat/json-rpc/v1"

newtype JsonRequestHeartbeat = JsonRequestHeartbeat
        { _jrhPreferredTimeoutSeconds :: Maybe Int } deriving (Show, Generic)
instance A.ToJSON JsonRequestHeartbeat where
    toJSON = A.genericToJSON defaultToJsonOptions

data JsonActionPerformed = JAP_NONE
                         | JAP_CANCELLATION_REQUEST_SUBMITTED
                         | JAP_ALL_BETS_CANCELLED
                         | JAP_SOME_BETS_NOT_CANCELLED
                         | JAP_CANCELLATION_REQUEST_ERROR
                         | JAP_CANCELLATION_STATUS_UNKNOWN
                         deriving (Show, Generic)
instance A.FromJSON JsonActionPerformed where
     parseJSON = A.genericParseJSON defaultEnumFromJsonOptions
instance A.ToJSON JsonActionPerformed where
     toJSON = A.genericToJSON defaultToJsonOptions

data JsonHeartbeatReport = JsonHeartbeatReport
        { _jhbrActionPerformed      :: Maybe JsonActionPerformed
        , _jhbrActualTimeoutSeconds :: Maybe Int
        } deriving (Show, Generic)
instance A.FromJSON JsonHeartbeatReport where
     parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonHeartbeatReport where
     toJSON = A.genericToJSON defaultToJsonOptions

-- | Heartbeat API
data HeartbeatHandler m a where
    GetHeartbeatTimeout :: HeartbeatHandler m NominalDiffTime

makeSem ''HeartbeatHandler

