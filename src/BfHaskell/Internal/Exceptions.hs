-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.Internal.Exceptions
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.Internal.Exceptions
(
    showException
) where

import qualified Control.Exception as E

showException :: E.SomeException -> String
showException = show

