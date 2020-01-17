-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.DSL.Heartbeat
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.DSL.Heartbeat
(
  -- * Effect
  HeartbeatHandler(..)

  -- * Actions
  , getHeartbeatTimeout

 -- * Interpretations
  , runHeartbeatHandler

 -- * Helpers
  , defaultHeartbeatUrl
) where

import           BfHaskell.HeartbeatAPI.Heartbeat (runHeartbeatHandler)
import           BfHaskell.HeartbeatAPI.Types     (HeartbeatHandler (..),
                                                   defaultHeartbeatUrl,
                                                   getHeartbeatTimeout)
