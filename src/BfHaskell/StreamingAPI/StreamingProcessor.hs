-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.StreamingProcessor
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.StreamingProcessor
(
    runStreamingHandler
) where

import           BfHaskell.Common.Logging
import           BfHaskell.DSL.Login                    (LoginHandler)
import           BfHaskell.StreamingAPI.CommandExecutor (runCommandExecutor)
import           BfHaskell.StreamingAPI.SslClient       (runSslClient)
import           BfHaskell.StreamingAPI.StreamingComm   (CommCentre, StreamingControlMessage (..),
                                                         addControlMessage,
                                                         ccMarketCache,
                                                         ccOrderCache,
                                                         newCommCentre,
                                                         readClientUpdate,
                                                         tryReadClientUpdate)
import           BfHaskell.StreamingAPI.Types           (StreamingConnectionInfo (..),
                                                         StreamingHandler (..),
                                                         scStore)
import qualified Control.Concurrent.Async               as AS
import           Control.Concurrent.STM.TVar            (readTVarIO)
import           Control.Lens                           (view)
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (liftIO)
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Resource

-- | Runs 'StreamingHandler' interpreter
runStreamingHandler :: Members '[Embed IO,
                                 Output LogMessage,
                                 Async,
                                 Resource,
                                 LoginHandler,
                                 Error String] r
                    => StreamingConnectionInfo  -- ^ Streaming configuration
                    -> InterpreterFor StreamingHandler r
runStreamingHandler connectionInfo sem = do
    comm <- liftIO newCommCentre
    bracket (startThreads comm) (stopThreads comm) (const $ interpretStreaming comm)
  where
    startThreads comm = do
        -- Run SSL connection thread
        sslClientThread <- async $ runSslClient comm connectionInfo
        -- Run control command execution thread
        cmdExecutorThread <- async $ runCommandExecutor sslClientThread comm
        return (sslClientThread, cmdExecutorThread)

    -- This signature needs to be in place in order to infer types
    stopThreads :: Members '[Embed IO,
                             Async,
                             Resource,
                             LoginHandler,
                             Output LogMessage,
                             Error String] r
                 => CommCentre
                 -> (AS.Async (Maybe ()), AS.Async (Maybe ()))
                 -> Sem r ()
    stopThreads comm (sslClientThread, cmdExecutorThread) = do
        liftIO $ addControlMessage comm SCMStopService
        void $ await sslClientThread
        void $ await cmdExecutorThread

    interpretStreaming comm =
        interpret (\case
            GetNextStreamMessage -> readClientUpdate comm
            TryGetNextStreamMessage -> tryReadClientUpdate comm
            SubscribeToMarkets marketFilter ->
                addControlMessage comm $ SCMSubscribeToMarkets  marketFilter
            GetMarketCache -> do
                cache <- embed $ readTVarIO $ view ccMarketCache comm
                return $ view scStore cache
            SubscribeToOrders -> addControlMessage comm SCMSubscribeToOrders
            GetOrderCache -> do
                cache <- embed $ readTVarIO $ view ccOrderCache comm
                return $ view scStore cache
            ) sem
