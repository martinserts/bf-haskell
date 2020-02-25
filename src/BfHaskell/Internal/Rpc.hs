-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.Internal.Rpc
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE DeriveGeneric #-}

module BfHaskell.Internal.Rpc
(
    JsonRpcRequest(..), JsonAPINGException(..), JsonRpcResponse(..)
  , performRpcRequest
) where

import           BfHaskell.Common.Logging
import           BfHaskell.DSL.Login           (LoginHandler, fetchToken,
                                                getAppKey)
import           BfHaskell.Internal.Exceptions (showException)
import           BfHaskell.Internal.JsonTypes  (defaultFromJsonOptions,
                                                defaultToJsonOptions,
                                                toJsonText)
import           BfHaskell.Internal.Network    (addHeader)
import           BfHaskell.LoginAPI.Types      (SessionToken (..))
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    ((.:))
import qualified Data.Aeson                    as A
import           Data.Text                     (Text)
import qualified Data.UUID                     as UUID
import           Data.UUID.V4                  as UUID
import           GHC.Generics                  (Generic)
import           Network.HTTP.Req              (HttpConfig, Option, POST (..),
                                                ReqBodyJson (..),
                                                Scheme (Https), Url,
                                                jsonResponse, req, responseBody,
                                                runReq)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader

data JsonRpcRequest = JsonRpcRequest { _jrpcId      :: Text
                                     , _jrpcMethod  :: Text
                                     , _jrpcJsonrpc :: Text
                                     , _jrpcParams  :: A.Value
                                     } deriving (Show, Generic)
instance A.ToJSON JsonRpcRequest where
        toJSON = A.genericToJSON defaultToJsonOptions

data JsonAPINGException = JsonAPINGException { _jaexErrorDetails :: Text
                                             , _jaexErrorCode    :: Text
                                             , _jaexRequestUUID  :: Text
                                             } deriving (Show, Generic)
instance A.FromJSON JsonAPINGException where
        parseJSON = A.withObject "JsonAPINGException" $ \o -> do
            d <- o .: "data"
            exceptionname <- d .: "exceptionname"
            exn <- d .: exceptionname
            errorDetails <- exn .: "errorDetails"
            errorCode <- exn .: "errorCode"
            requestUUID <- exn .: "requestUUID"
            return $ JsonAPINGException errorDetails errorCode requestUUID
instance A.ToJSON JsonAPINGException where
        toJSON = A.genericToJSON defaultToJsonOptions

data JsonRpcResponse = JsonRpcResponse { _jrprId      :: Text
                                       , _jrprJsonrpc :: Text
                                       , _jrprResult  :: Maybe A.Value
                                       , _jrprError   :: Maybe JsonAPINGException
                                       } deriving (Show, Generic)
instance A.FromJSON JsonRpcResponse  where
        parseJSON = A.genericParseJSON defaultFromJsonOptions
instance A.ToJSON JsonRpcResponse where
        toJSON = A.genericToJSON defaultToJsonOptions


extractApiResponse :: (A.FromJSON a, Member (Error String) r)
                   => JsonRpcResponse
                   -> Sem r a
extractApiResponse r =
    case (_jrprResult r, _jrprError r) of
      (_, Just e)   -> throw $ show e
      (Just res, _) -> case A.fromJSON res of
                         A.Error e   -> throw e
                         A.Success s -> return s
      _             -> throw "Both - response and error empty"

performRpcRequest :: (A.ToJSON a,
                      A.FromJSON b,
                      Members '[Embed IO,
                                LoginHandler,
                                Reader HttpConfig,
                                Output LogMessage,
                                Error String] r)
                  => Url 'Https     -- ^ Url
                  -> Option 'Https  -- ^ Options
                  -> Text           -- ^ API command
                  -> a              -- ^ JSON request
                  -> Sem r b
performRpcRequest url options apiCommand request = do
    guid <- liftIO UUID.nextRandom
    let rpcRequest = JsonRpcRequest (UUID.toText guid) apiCommand "2.0"
                                    $ A.toJSON request
    -- Log request
    logDebug $ toJsonText rpcRequest

    appKey <- getAppKey
    SessionToken token <- fetchToken
    let headers = options `addHeader` ("X-Application", appKey)
                          `addHeader` ("X-Authentication", token)

    httpConfig <- ask
    let request' = req POST url (ReqBodyJson rpcRequest) jsonResponse headers
    response <- fromExceptionVia showException
                    $ runReq httpConfig request'
    let rpcResponse :: JsonRpcResponse = responseBody response

    -- Log response
    logDebug $ toJsonText rpcResponse

    extractApiResponse rpcResponse
