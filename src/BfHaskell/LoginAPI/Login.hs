-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.LoginAPI.Login
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module BfHaskell.LoginAPI.Login
(
    runLoginHandler
  , newLoginCredentials
) where

import           BfHaskell.Common.Logging
import           BfHaskell.Internal.Exceptions (showException)
import           BfHaskell.Internal.JsonTypes  (defaultFromJsonOptions)
import           BfHaskell.Internal.Network    (addHeader, makeTlsClientManager,
                                                parseUrl)
import           BfHaskell.LoginAPI.Types      (LoginCredentials (..),
                                                LoginHandler (..),
                                                SessionToken (..),
                                                defaultLoginUrl)
import           Control.Concurrent.STM
import           Control.Monad                 (guard)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Aeson                    as A
import           Data.Default
import           Data.Either                   (either)
import           Data.Text                     (Text, pack)
import qualified Data.Text.IO                  as TIO
import           Data.Time                     (UTCTime, diffUTCTime,
                                                getCurrentTime)
import           Data.Time.Clock               (NominalDiffTime)
import           GHC.Generics                  (Generic)
import           Network.HTTP.Req              (HttpConfig (httpConfigAltManager),
                                                Option, POST (..), Req,
                                                ReqBodyUrlEnc (..),
                                                Scheme (Https), Url,
                                                defaultHttpConfig, jsonResponse,
                                                req, responseBody, runReq, (=:))
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader


data JsonLoginResponse = JsonLoginResponse { _jslrSessionToken :: Text
                                           , _jslrLoginStatus  :: Text
                                           } deriving (Show, Generic)
instance A.FromJSON JsonLoginResponse where
    parseJSON = A.genericParseJSON defaultFromJsonOptions


data SessionTokenWithTime = SessionTokenWithTime
    { _stwtToken            :: SessionToken
    , _stwtTokenRetrievedAt :: UTCTime
    } deriving (Show)

data LoginSettings = LoginSettings
        { _lsCreds :: LoginCredentials
        , _lsToken :: TVar (Maybe SessionTokenWithTime)
        }

newtype LoginHttpConfig = LoginHttpConfig HttpConfig


createHttpConfig :: Members '[Embed IO, Error String] r
                 => Text        -- ^ Public certificate
                 -> Text        -- ^ Private key
                 -> Sem r HttpConfig
createHttpConfig publicCertificate privateKey = do
    manager <- makeTlsClientManager "identitysso" publicCertificate privateKey
    return $ defaultHttpConfig { httpConfigAltManager = Just manager}

fetchSessionToken :: Members '[Embed IO,
                               Output LogMessage,
                               Reader LoginSettings,
                               Reader LoginHttpConfig,
                               Error String] r
                  => Sem r SessionToken
fetchSessionToken = do
    creds <- asks _lsCreds
    (url, option) <- parseUrl $ _lcLoginUrl creds
    let request = createLoginRequest url option (_lcUsername creds)
                                                (_lcPassword creds)
                                                (_lcAppKey creds)
    (LoginHttpConfig httpConfig) <- ask
    response <- fromExceptionVia showException
                    $ runReq httpConfig request
    logDebug $ either (pack . ("Failed to fetch token: " <>))
                      (const "Successfully fetched token") response

    fromEither response

createLoginRequest :: Url 'Https
                   -> Option 'Https
                   -> Text              -- ^ Username
                   -> Text              -- ^ Password
                   -> Text              -- ^ Application name
                   -> Req (Either String SessionToken)
createLoginRequest url defaultOptions username password appName = do
    let options = defaultOptions `addHeader` ("X-Application", appName)
    response <- req POST url (ReqBodyUrlEnc params) jsonResponse options
    let loginResponse = responseBody response
    let result = if _jslrLoginStatus loginResponse == "SUCCESS" then
                    Right $ SessionToken $ _jslrSessionToken loginResponse
                 else Left $ show loginResponse
    return result
  where
      params = "username" =: username <> "password" =: password

fetchTokenThroughCache :: Members '[Embed IO,
                                    Output LogMessage,
                                    Reader LoginSettings,
                                    Reader LoginHttpConfig,
                                    Error String] r
                       => Sem r SessionToken
fetchTokenThroughCache = do
    currentTime <- liftIO getCurrentTime
    tvarToken <- asks _lsToken
    mToken <- liftIO $ readTVarIO tvarToken
    expiry <- asks $ _lcExpiry . _lsCreds

    case getCachedToken currentTime mToken expiry of
      Just token -> return token
      Nothing -> do
          token <- fetchSessionToken
          liftIO . atomically . writeTVar tvarToken . Just
                 $ SessionTokenWithTime token currentTime
          return token
  where
    getCachedToken currentTime mToken expiry = do
        SessionTokenWithTime token retrievedAt <- mToken
        guard $ diffUTCTime currentTime retrievedAt < expiry
        return token

readCertificate :: Members '[Embed IO, Error String] r
                => FilePath         -- ^ Path to certificate file
                -> Sem r Text
readCertificate fileName = fromExceptionVia showException $ TIO.readFile fileName

newLoginCredentials :: Members '[Embed IO, Error String] r
                    => Text     -- ^ Username
                    -> Text     -- ^ Password
                    -> Text     -- ^ Secret application key
                    -> FilePath -- ^ Path to public certificate (PEM format)
                    -> FilePath -- ^ Path to private key (RSA PEM format)
                    -> NominalDiffTime -- ^ Token expires in seconds
                    -> Sem r LoginCredentials
newLoginCredentials username password appKey pubCert privCert expiry = do
    pub <- readCertificate pubCert
    priv <- readCertificate privCert
    return $ LoginCredentials username password appKey pub priv defaultLoginUrl expiry

-- | Runs 'LoginHandler' interpreter
runLoginHandler :: Members [Embed IO, Output LogMessage, Error String] r
                => LoginCredentials     -- ^ Login credentials. Use 'newLoginCredentials' helper to populate this data structure.
                -> Maybe HttpConfig     -- ^ Override 'HttpConfig' if needed. Use 'Nothing' for default configuration.
                -> InterpreterFor LoginHandler r
runLoginHandler creds httpConfig sem = do
    httpConfig' <- maybe newHttpConfig pure httpConfig
    settings <- LoginSettings <$> pure creds <*> liftIO (newTVarIO def)

    runReader settings
      . runReader (LoginHttpConfig httpConfig')
      $ reinterpret2 (\case
          FetchToken -> fetchTokenThroughCache
          GetAppKey -> return $ _lcAppKey creds
          GetExpiry -> return $ _lcExpiry creds
        ) sem
  where
    newHttpConfig = createHttpConfig (_lcPublicCertificate creds)
                                     (_lcPrivateKey creds)
