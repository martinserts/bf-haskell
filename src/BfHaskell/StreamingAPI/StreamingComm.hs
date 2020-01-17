-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.StreamingComm
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE TemplateHaskell #-}

module BfHaskell.StreamingAPI.StreamingComm
(
    StreamingControlMessage (..)
  , CommCentre (..)
  , ccMarketCache, ccOrderCache, ccUpdates, ccControl, ccConnection, ccLastMessageId
  , newCommCentre
  , modifyMarketCache, modifyOrderCache
  , addClientUpdate
  , addControlMessage
  , readClientUpdate, tryReadClientUpdate
  , readControlMessage
  , readConnection, storeConnection
  , getNextMessageId
) where

import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.StreamingMarkets (MarketCache)
import           BfHaskell.StreamingAPI.StreamingOrders  (OrderCache)
import           BfHaskell.StreamingAPI.Types            (StreamingMessage)
import           Control.Concurrent.STM                  (atomically)
import           Control.Concurrent.STM.TBQueue          (TBQueue, newTBQueue,
                                                          readTBQueue,
                                                          tryReadTBQueue,
                                                          writeTBQueue)
import           Control.Concurrent.STM.TVar             (TVar, newTVar,
                                                          readTVar, readTVarIO,
                                                          writeTVar)
import           Control.Lens                            (makeLenses)
import           Control.Monad.IO.Class                  (MonadIO, liftIO)
import           Data.Default
import           System.IO.Streams.TLS                   (TLSConnection)

data StreamingControlMessage = SCMStopService
                             | SCMSubscribeToMarkets MarketFilter
                             | SCMSubscribeToOrders
    deriving (Show)

-- | Contains market and order caches and communication queues
-- Used by both - stream processor and client
data CommCentre = CommCentre
    { _ccMarketCache   :: TVar MarketCache
    , _ccOrderCache    :: TVar OrderCache
    , _ccUpdates       :: TBQueue StreamingMessage
    , _ccControl       :: TBQueue StreamingControlMessage
    , _ccConnection    :: TVar (Maybe TLSConnection)
    , _ccLastMessageId :: TVar Int
    }

makeLenses ''CommCentre

-- | Creates the default comm centre
newCommCentre :: MonadIO m => m CommCentre
newCommCentre =
    let defaultUpdateQueueSize = 1000
        defaultControlQueueSize = 10
    in liftIO $ atomically $ CommCentre <$> newTVar def
                                        <*> newTVar def
                                        <*> newTBQueue defaultUpdateQueueSize
                                        <*> newTBQueue defaultControlQueueSize
                                        <*> newTVar def
                                        <*> newTVar def

modifyCache :: MonadIO m
            => (CommCentre -> TVar c)
            -> CommCentre
            -> (c -> (c, a))
            -> m a
modifyCache f comm runState =
    liftIO $ atomically $ do
        let t = f comm
        cache <- readTVar t
        let (cache', changes) = runState cache
        writeTVar t cache'
        return changes

-- | Modify market cache
modifyMarketCache :: MonadIO m
                  => CommCentre
                  -> (MarketCache -> (MarketCache, a))
                  -> m a
modifyMarketCache = modifyCache _ccMarketCache

-- | Modify market cache
modifyOrderCache :: MonadIO m
                  => CommCentre
                  -> (OrderCache -> (OrderCache, a))
                  -> m a
modifyOrderCache = modifyCache _ccOrderCache

addToQueue :: MonadIO m => (CommCentre -> TBQueue a) -> CommCentre -> a -> m ()
addToQueue f comm a = liftIO $ atomically $ writeTBQueue (f comm) a

-- | Add client update to queue
addClientUpdate :: MonadIO m => CommCentre -> StreamingMessage -> m ()
addClientUpdate = addToQueue _ccUpdates

-- | Add control message to queue
addControlMessage :: MonadIO m => CommCentre -> StreamingControlMessage -> m ()
addControlMessage = addToQueue _ccControl

readQueue :: MonadIO m => (CommCentre -> TBQueue a) -> CommCentre -> m a
readQueue f comm = liftIO $ atomically $ readTBQueue $ f comm

tryReadQueue :: MonadIO m => (CommCentre -> TBQueue a) -> CommCentre -> m (Maybe a)
tryReadQueue f comm = liftIO $ atomically $ tryReadTBQueue $ f comm

readClientUpdate :: MonadIO m => CommCentre -> m StreamingMessage
readClientUpdate = readQueue _ccUpdates

tryReadClientUpdate :: MonadIO m => CommCentre -> m (Maybe StreamingMessage)
tryReadClientUpdate = tryReadQueue _ccUpdates

readControlMessage :: MonadIO m => CommCentre -> m StreamingControlMessage
readControlMessage = readQueue _ccControl

readConnection :: MonadIO m => CommCentre -> m (Maybe TLSConnection)
readConnection = liftIO . readTVarIO . _ccConnection

storeConnection :: MonadIO m => CommCentre -> Maybe TLSConnection -> m ()
storeConnection comm c = liftIO $ atomically $ writeTVar (_ccConnection comm) c

getNextMessageId :: MonadIO m => CommCentre -> m Int
getNextMessageId comm =
    liftIO $ atomically $ do
        let t = _ccLastMessageId comm
        nextId <- succ <$> readTVar t
        writeTVar t nextId
        return nextId
