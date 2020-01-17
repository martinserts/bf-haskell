-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.DSL.Login
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.DSL.Login
(
  -- * Effect
  LoginHandler(..)

  -- * Actions
  , fetchToken
  , getAppKey
  , getExpiry

 -- * Interpretations
  , runLoginHandler

-- * Helpers
  , newLoginCredentials
  , defaultLoginUrl

  -- * Data types
  , LoginCredentials(..)
  , SessionToken(..)
) where

import           BfHaskell.LoginAPI.Login (newLoginCredentials, runLoginHandler)
import           BfHaskell.LoginAPI.Types (LoginCredentials (..),
                                           LoginHandler (..), SessionToken (..),
                                           defaultLoginUrl, fetchToken,
                                           getAppKey, getExpiry)
