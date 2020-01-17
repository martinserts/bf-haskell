-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.LoginAPI.Types
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE TemplateHaskell #-}

module BfHaskell.LoginAPI.Types
(
    LoginCredentials(..)
  , SessionToken(..)
  , LoginHandler(..), fetchToken, getAppKey, getExpiry
  , defaultLoginUrl
) where

import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Polysemy


-- | Default login url
defaultLoginUrl :: Text
defaultLoginUrl = "https://identitysso-cert.betfair.com/api/certlogin"

-- | Details required for Betfair login
data LoginCredentials = LoginCredentials
        { _lcUsername          :: Text  -- ^ Username
        , _lcPassword          :: Text  -- ^ Password
        , _lcAppKey            :: Text  -- ^ Secret application key
        , _lcPublicCertificate :: Text  -- ^ Public certificate (PEM format)
        , _lcPrivateKey        :: Text  -- ^ Private key (RSA PEM format)
        , _lcLoginUrl          :: Text  -- ^ Login URL
        , _lcExpiry            :: NominalDiffTime -- ^ Token expires in seconds
        }

-- | Sesssion token retrieved using Login API
newtype SessionToken = SessionToken Text
    deriving (Show)

-- | Login DSL
data LoginHandler m a where
    FetchToken :: LoginHandler m SessionToken   -- ^ Fetch session token by logging in
    GetAppKey :: LoginHandler m Text            -- ^ Get configured app secret
    GetExpiry :: LoginHandler m NominalDiffTime -- ^ Get configured expiry interval

makeSem_ ''LoginHandler

-- | Fetches session token by logging in
fetchToken :: Member LoginHandler r => Sem r SessionToken

-- | Gets configured app secret
getAppKey :: Member LoginHandler r => Sem r Text

-- | Gets configured expiry interval
getExpiry :: Member LoginHandler r => Sem r NominalDiffTime
