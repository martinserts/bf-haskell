-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.SslClient
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module BfHaskell.StreamingAPI.SslClient
(
    runSslClient
  , defaultStreamingConnectionInfo
) where

import           BfHaskell.Common.Logging
import           BfHaskell.Common.Odds                   (OddsTree, newTree)
import           BfHaskell.DSL.Login                     (LoginHandler,
                                                          SessionToken (..),
                                                          fetchToken, getAppKey)
import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.StreamingComm    (CommCentre,
                                                          addClientUpdate,
                                                          getNextMessageId,
                                                          modifyMarketCache,
                                                          modifyOrderCache,
                                                          storeConnection)
import           BfHaskell.StreamingAPI.StreamingMarkets (cleanupMarket,
                                                          extractMarketChanges)
import           BfHaskell.StreamingAPI.StreamingOrders  (cleanupOrder,
                                                          extractOrderChanges)
import           BfHaskell.StreamingAPI.StreamingUtils   (sendStreamMessage,
                                                          updateStateProperty)
import           BfHaskell.StreamingAPI.Types            (MarketDetails,
                                                          MarketId,
                                                          OrderRunnerTable,
                                                          SMConnectionState (SMCSConnected, SMCSDisconnected),
                                                          StreamCache,
                                                          StreamMessageParser (..),
                                                          StreamingConnectionInfo (..),
                                                          StreamingMessage (..),
                                                          StreamingState (..),
                                                          crlf, scClk,
                                                          scConflateMs,
                                                          scHeartbeatMs,
                                                          scInitialClk, scPt,
                                                          scSegments, scStatus,
                                                          scStore,
                                                          scSubscriptionId,
                                                          ssAuthMsgId,
                                                          ssConnectionId,
                                                          ssLastCleanup,
                                                          ssStreamBuffer)
import           Control.Lens                            (over, set, view)
import           Control.Monad                           (forM, guard, join,
                                                          void, when)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Retry                           (constantDelay,
                                                          retrying)
import qualified Data.Aeson                              as A
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString                         as B
import           Data.Connection                         (Connection (close, source))
import           Data.Default
import           Data.Foldable                           (toList)
import qualified Data.Map.Strict                         as M
import           Data.Maybe                              (catMaybes, fromMaybe,
                                                          isNothing)
import qualified Data.Sequence                           as Seq
import qualified Data.Text                               as T
import           Data.Time.Clock                         (addUTCTime,
                                                          getCurrentTime)
import           Data.TLSSetting                         (TrustedCAStore (SystemCAStore),
                                                          makeClientParams)
import           Network.TLS                             (ClientParams (..), MaxFragmentEnum (MaxFragment4096))
import           Polysemy
import           Polysemy.Error
import           Polysemy.NonDet
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.Resource
import           Polysemy.State
import qualified System.IO.Streams                       as IOS
import           System.IO.Streams.TLS                   (TLSConnection,
                                                          connect)

class StreamChangeExtractor c k v | c -> k v where
    extractChanges :: Members '[State (StreamCache c k v), Reader OddsTree] r
                   => c
                   -> Sem r (Maybe k)
    cleanup :: Member (State (StreamCache c k v)) r => Sem r ()

instance StreamChangeExtractor MarketChange MarketId MarketDetails where
    extractChanges = extractMarketChanges
    cleanup = cleanupMarket

instance StreamChangeExtractor OrderMarketChange MarketId OrderRunnerTable where
    extractChanges = extractOrderChanges
    cleanup = cleanupOrder


data StreamReadResult = SRRGotLine StreamResponse
                      | SRRFinished
                      deriving (Show)

-- | Default streaming connection info
defaultStreamingConnectionInfo :: StreamingConnectionInfo
defaultStreamingConnectionInfo =
    StreamingConnectionInfo "stream-api.betfair.com" 443

runSslClient :: Members '[Embed IO,
                          Resource,
                          LoginHandler,
                          Output LogMessage,
                          Error String] r
             => CommCentre
             -> StreamingConnectionInfo
             -> Sem r ()
runSslClient comm connectionInfo = do
    logDebug "runSslClient - starting"
    retrying (constantDelay delay) alwaysRetry startAction
    logDebug "runSslClient - finished"
  where
    delay = 5 * 1000000 -- 5 seconds between each reconnection
    alwaysRetry _ _ = return True

    startAction _rs =
        void . runState def
             . runReader comm
             . runReader newTree
             $ connectAndAuthenticate connectionInfo

connectAndAuthenticate :: Members '[Embed IO,
                                    Output LogMessage,
                                    Resource,
                                    Reader CommCentre,
                                    Reader OddsTree,
                                    State StreamingState,
                                    LoginHandler,
                                    Error String] r
                       => StreamingConnectionInfo
                       -> Sem r ()
connectAndAuthenticate (StreamingConnectionInfo hostName port) = do
    logInfo $ mconcat ["connectAndAuthenticate ["
                       , hostName
                       , ":"
                       , T.pack . show $ port
                       , "] - starting"]

    comm <- ask

    bracket
        (do
            -- Connect to TLS stream
            clientParams' <- liftIO $ makeClientParams SystemCAStore
            let clientParams = clientParams' { clientUseMaxFragmentLength = Just MaxFragment4096 }
            let p = fromInteger . toInteger $ port
            conn <- liftIO $ connect clientParams Nothing (T.unpack hostName) p

            -- Preserve connection in storage
            liftIO $ storeConnection comm $ Just conn

            return conn
        ) -- Acquire TCP stream
        (\conn -> do
            liftIO $ do
                close conn
                -- Remove connection from storage
                storeConnection comm Nothing

            -- Send update message to client
            addClientUpdate comm $ SMConnectionStateChanged SMCSDisconnected
        ) -- Release TCP stream
        processStream

    logDebug "connectAndAuthenticate - finished"


-- | Cleanup order and market caches
runCleanup :: Members '[Embed IO,
                        Reader CommCentre,
                        State StreamingState] r
           => Sem r ()
runCleanup = do
    state <- get
    currentTime <- liftIO getCurrentTime

    let timeout = 60 * 5 -- 5 minutes
        expiryTime = addUTCTime timeout <$> _ssLastCleanup state
        expired = maybe True (< currentTime) expiryTime

    when expired $ do
        comm <- ask
        liftIO $ do
            -- Run Market cleanup
            modifyMarketCache comm $ \mc ->
                run . runState mc $ cleanup
            -- Run Order cleanup
            modifyOrderCache comm $ \oc ->
                run . runState oc $ cleanup
        -- Update last cleanup date
        modify' . set ssLastCleanup $ Just currentTime

-- | Processes stream until it is interrupted
processStream :: Members '[Embed IO,
                          Output LogMessage,
                          Reader CommCentre,
                          Reader OddsTree,
                          State StreamingState,
                          LoginHandler,
                          Error String] r
              => TLSConnection
              -- ^ Tcp connection
              -> Sem r ()
processStream conn = do
    logDebug "processStream - starting"
    go
  where
    go = do
        runCleanup

        res <- fetchLine conn
        logDebug $ T.pack $ show res
        case res of
          SRRGotLine line -> processLine conn line >> go
          SRRFinished     -> return ()

getCrLfLine :: ByteString
            -> Maybe (ByteString, ByteString)
            -- ^ (Json, Remaining)
getCrLfLine full =
    let (h, t) = B.breakSubstring crlf full
    in if B.null t then Nothing else
        Just (h, B.drop (B.length crlf) t)

-- | Fetches next Json message
-- Blocks to read from network if there is no data in buffer
fetchLine :: Members '[Embed IO, State StreamingState, Error String] r
          => TLSConnection            -- ^ TCP connection
          -> Sem r StreamReadResult
fetchLine conn = do
    streamResponse <- parseBuffer
    case streamResponse of
      Just line -> return $ SRRGotLine line
      Nothing   -> do
          mbytes <- liftIO $ IOS.read $ source conn
          case mbytes of
            Just bytes -> do
                modify' $ over ssStreamBuffer (`B.append` bytes)
                fetchLine conn
            Nothing -> return SRRFinished

-- | Parses Json message in state buffer
-- Returns Nothing if there is no full message available
parseBuffer :: Members '[State StreamingState, Error String] r
            => Sem r (Maybe StreamResponse)
parseBuffer = do
    buffer <- gets _ssStreamBuffer
    forM (getCrLfLine buffer) $ \(json, remaining) -> do
      modify' $ set ssStreamBuffer remaining
      fromEither $ A.eitherDecodeStrict json

processLine :: Members '[Embed IO,
                         Output LogMessage,
                         Reader CommCentre,
                         Reader OddsTree,
                         State StreamingState,
                         LoginHandler,
                         Error String] r
            => TLSConnection
            -> StreamResponse
            -> Sem r ()
processLine conn (SRConnectionMessage cm)   = do
    modify' $ set ssConnectionId $ connectionMessageConnectionId cm
    SessionToken token <- fetchToken
    appKey <- getAppKey

    msgId <- getNextMessageId =<< ask
    modify' $ set ssAuthMsgId $ Just msgId

    sendStreamMessage conn
        mkAuthenticationMessage
        { authenticationMessageId = Just msgId
        , authenticationMessageSession = Just token
        , authenticationMessageAppKey = Just appKey
        }

processLine _conn (SRStatusMessage sm)       = do
    comm <- ask
    authMsgId <- gets $ view ssAuthMsgId
    when (authMsgId == statusMessageId sm &&
          statusMessageStatusCode sm == Just E'StatusCode'SUCCESS) $
              addClientUpdate comm $ SMConnectionStateChanged SMCSConnected

processLine _conn (SROrderChangeMessage om)  = do
    logDebug $ "ProcessLine: " <> (T.pack . show $ om)

    comm <- ask
    oddsTree <- ask
    changes <- liftIO $ modifyOrderCache comm (updateOrderCache oddsTree)

    case changes of
      Nothing -> return ()
      Just c  -> liftIO $ addClientUpdate comm $ SMOrderUpdate c

  where
    updateOrderCache oddsTree orderCache =
        run . runState orderCache
            . runReader oddsTree
            . runNonDet $ processMessage om

processLine _conn (SRMarketChangeMessage mm) = do
    logDebug $ "ProcessLine: " <> (T.pack . show $ mm)

    comm <- ask
    oddsTree <- ask
    changes <- liftIO $ modifyMarketCache comm (updateMarketCache oddsTree)

    case changes of
      Nothing -> return ()
      Just c  -> liftIO $ addClientUpdate comm $ SMMarketUpdate c

  where
    updateMarketCache oddsTree marketCache =
        run . runState marketCache
            . runReader oddsTree
            . runNonDet $ processMessage mm

-- | Processes MarketChange and OrderMarketChange messages, and updates state
-- accordingly.
-- Returns table of changes
processMessage :: forall msg c k v r.
                  (StreamMessageParser msg c, StreamChangeExtractor c k v,
                   Members '[NonDet, Reader OddsTree, State (StreamCache c k v)] r)
               => msg
               -> Sem r [k]
processMessage msg = do
    -- Ignore messages from previous subscriptions
    subscriptionId <- gets $ view scSubscriptionId
    guard $ Just subscriptionId == getMessageId msg

    -- Update global properties
    updateStateProperty scInitialClk $ getInitialClk msg
    updateStateProperty scClk $ getClk msg
    updateStateProperty scHeartbeatMs $ getHeartbeatMs msg
    updateStateProperty scConflateMs $ getConflateMs msg
    updateStateProperty scPt $ getPt msg
    updateStateProperty scStatus $ getStatus msg

    -- Clear store if segment starts for sub image
    when (changeType == Just E'Ct'SUB_IMAGE
       && (segmentType == Just E'SegmentType'SEG_START || isNothing segmentType))
        $ modify' $ set scStore M.empty

    -- Accumulate changes if segmentation is applied,
    -- otherwise apply changes to cache at once
    case segmentType of
      Nothing -> applyChanges changes
      Just st ->
          case st of
            E'SegmentType'SEG_START -> do
                modify' $ set scSegments $ Seq.singleton changes
                return []
            E'SegmentType'SEG       -> do
                modify' $ over scSegments (Seq.|> changes)
                return []
            E'SegmentType'SEG_END   -> do
                modify' $ over scSegments (Seq.|> changes)
                seg <- gets $ view scSegments
                applyChanges $ join . toList $ seg

  where
    changeType = getCt msg
    segmentType = getSegmentType msg
    changes = fromMaybe [] $ getChanges msg

    applyChanges c = catMaybes <$> traverse extractChanges c
