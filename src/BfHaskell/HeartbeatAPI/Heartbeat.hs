-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.HeartbeatAPI.Heartbeat
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.HeartbeatAPI.Heartbeat
(
    runHeartbeatHandler
) where

import           BfHaskell.Common.Logging
import           BfHaskell.DSL.Login          (LoginHandler)
import           BfHaskell.HeartbeatAPI.Types (HeartbeatHandler (..),
                                               JsonHeartbeatReport,
                                               JsonRequestHeartbeat (..))
import           BfHaskell.Internal.Network   (parseUrl)
import           BfHaskell.Internal.Rpc       (performRpcRequest)
import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.Async     as AS
import           Control.Monad                (forever, void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text, pack)
import           Data.Time.Clock              (NominalDiffTime)
import           Network.HTTP.Req             (HttpConfig, Option,
                                               Scheme (Https), Url,
                                               defaultHttpConfig)
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.Resource


data HeartbeatConfig = HeartbeatConfig
        { _hcUrl     :: Url 'Https
        , _hcOptions :: Option 'Https
        , _hcTimeout :: NominalDiffTime
        }


runHeartbeatHandler :: Members '[Embed IO,
                                 LoginHandler,
                                 Output LogMessage,
                                 Async,
                                 Resource,
                                 Error String] r
                    => Text              -- ^ Hearbeat API url
                    -> Maybe HttpConfig  -- ^ Override 'HttpConfig' if needed. Use 'Nothing' for default configuration.
                    -> NominalDiffTime   -- ^ Timeout applied to hearbeat
                    -> Sem (HeartbeatHandler ': r) a
                    -> Sem r a
runHeartbeatHandler url httpConfig timeout sem = do
    let httpConfig' = fromMaybe defaultHttpConfig httpConfig
    (url', options) <- parseUrl url
    let heartbeatConfig = HeartbeatConfig url' options timeout

    bracket (
        runReader httpConfig' $ async $ forever $ do
            -- Send heartbeat
            catch (void $ heartbeatRequest heartbeatConfig)
                  (logError . ("runHeartbeatHandler: heartbeatRequest failed - " <>) . pack)
            sleepBetweenCalls                       -- Sleep
        )                            -- Start heartbeat thread
        (liftIO . AS.cancel)         -- Kill hearbeat thread
        (const $ interpret (\case
                   GetHeartbeatTimeout -> return timeout
                 ) sem
        )
  where
    heartbeatRequest :: Members '[Embed IO,
                                  LoginHandler,
                                  Reader HttpConfig,
                                  Output LogMessage,
                                  Error String] r
                     => HeartbeatConfig -> Sem r JsonHeartbeatReport
    heartbeatRequest (HeartbeatConfig url' opts timeout') =
        let req = JsonRequestHeartbeat $ Just $ truncate timeout'
        in performRpcRequest url' opts "HeartbeatAPING/v1.0/heartbeat" req

    sleepBetweenCalls =
        let networkDelay = 5 -- Send requests this seconds earlier than timeout
        in embed $ threadDelay . truncate $ 1000000 * (timeout - networkDelay)

