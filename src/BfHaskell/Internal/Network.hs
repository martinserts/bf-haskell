-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.Internal.Network
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE OverloadedStrings #-}

module BfHaskell.Internal.Network
(
    makeClientManager
  , makeTlsClientManager
  , parseUrl
  , addHeader
) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Bifunctor           (first)
import           Data.Default.Class       (def)
import           Data.Text                (Text, unpack)
import qualified Data.Text.Encoding       as ENC
import           Network.Connection       (TLSSettings (TLSSettings))
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (mkManagerSettings)
import           Network.HTTP.Req         (Option, Scheme (Https), Url, header,
                                           useHttpsURI)
import           Network.TLS              (ClientHooks (onCertificateRequest, onServerCertificate),
                                           ClientParams (clientHooks, clientSupported),
                                           Supported (supportedCiphers),
                                           credentialLoadX509FromMemory,
                                           defaultParamsClient)
import           Network.TLS.Extra.Cipher (ciphersuite_default)
import           Polysemy
import           Polysemy.Error
import           Text.URI                 (mkURI)


createClientHooks :: ClientHooks
createClientHooks = def { onServerCertificate = \_ _ _ _ -> return [] }

createTlsClientHooks :: Text -- ^ Public certificate
                     -> Text -- ^ Private key
                     -> Either String ClientHooks
createTlsClientHooks publicCertificate privateKey = do
    creds <- credentialLoadX509FromMemory pc pk
    return $ createClientHooks { onCertificateRequest = \_ -> return $ Just creds }
  where
    pc = ENC.encodeUtf8 publicCertificate
    pk = ENC.encodeUtf8 privateKey

makeClientManager' :: Member (Embed IO) r
                   => Text          -- ^ Hostname
                   -> ClientHooks   -- ^ Hooks
                   -> Sem r Manager
makeClientManager' hostName hooks =
    liftIO $ newManager basicSettings
  where
    clientParams = (defaultParamsClient (unpack hostName) "")
        { clientHooks = hooks
        , clientSupported = def { supportedCiphers = ciphersuite_default }
        }
    basicSettings = mkManagerSettings (TLSSettings clientParams) Nothing

-- | Https client manager
makeClientManager :: Member (Embed IO) r => Text -> Sem r Manager
makeClientManager hostName = makeClientManager' hostName createClientHooks

-- | Https client manager with client certificate
makeTlsClientManager :: Members [Embed IO, Error String] r
                     => Text        -- ^ Hostname
                     -> Text        -- ^ Public certificate
                     -> Text        -- ^ Private key
                     -> Sem r Manager
makeTlsClientManager hostName publicCert privateKey = do
    hooks <- fromEither $ createTlsClientHooks publicCert privateKey
    makeClientManager' hostName hooks

-- | Parse HTTPS url
parseUrl :: Member (Error String) r
         => Text
         -> Sem r (Url 'Https, Option scheme)
parseUrl url = do
    uri <- fromEither . first show . mkURI $ url
    case useHttpsURI uri of
      Nothing     -> throw $ "Failed parsing HTTPS uri: " <> unpack url
      Just result -> return result

addHeader :: Option 'Https -> (Text, Text) -> Option 'Https
addHeader options (name, value) =
    options <> header (ENC.encodeUtf8 name) (ENC.encodeUtf8 value)

