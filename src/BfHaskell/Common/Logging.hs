-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.Common.Logging
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.Common.Logging
(
    LogMessage (..)
  , logMessage
  , logDebug
  , logInfo
  , logWarning
  , logError
) where

import           Colog.Core      (Severity (Debug, Error, Info, Warning))
import           Data.Text       (Text)
import           Polysemy
import           Polysemy.Output (Output, output)

data LogMessage = LogMessage Severity Text
    deriving (Show)

logMessage :: Member (Output LogMessage) r => LogMessage -> Sem r ()
logMessage = output

logDebug :: Member (Output LogMessage) r => Text -> Sem r ()
logDebug = logMessage . LogMessage Debug

logInfo :: Member (Output LogMessage) r => Text -> Sem r ()
logInfo = logMessage . LogMessage Info

logWarning :: Member (Output LogMessage) r => Text -> Sem r ()
logWarning = logMessage . LogMessage Warning

logError :: Member (Output LogMessage) r => Text -> Sem r ()
logError = logMessage . LogMessage Error
