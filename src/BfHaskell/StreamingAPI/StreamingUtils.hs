-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.StreamingUtils
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.StreamingUtils
(
    updateStateProperty
  , updateStreamingProperty
  , sendStreamMessage
) where

import           BfHaskell.Common.Logging
import           BfHaskell.Internal.Exceptions (showException)
import           BfHaskell.Internal.JsonTypes  (toJsonText)
import           BfHaskell.StreamingAPI.Types  (crlf)
import           Control.Lens                  (Lens', set)
import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy          (fromStrict)
import           Data.Connection               (Connection (send))
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.State
import           System.IO.Streams.TLS         (TLSConnection)

updateStateProperty :: Member (State s) r => Lens' s (Maybe a) -> Maybe a -> Sem r ()
updateStateProperty _ Nothing = return ()
updateStateProperty lens v    = modify' $ set lens v

updateStreamingProperty :: Lens' s (Maybe a) -> Maybe a -> s -> s
updateStreamingProperty _ Nothing state = state
updateStreamingProperty lens v state    = set lens v state

sendStreamMessage :: (A.ToJSON a, Members [Embed IO, Output LogMessage, Error String] r)
                  => TLSConnection -> a -> Sem r ()
sendStreamMessage conn msg = do
    logDebug $ "Sending: " <> toJsonText msg
    let m = A.encode msg <> fromStrict crlf
    fromExceptionVia showException $ send conn m
