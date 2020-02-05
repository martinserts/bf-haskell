-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.Types
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module BfHaskell.StreamingAPI.Types
(
    crlf
  , StreamCache(..)
  , scSubscriptionId, scClk, scInitialClk, scPt, scHeartbeatMs, scConflateMs
  , scStatus, scStore, scSegments
  , StreamMessageParser(..)
  , StreamingState(..)
  , ssStreamBuffer, ssConnectionId, ssAuthMsgId, ssLastCleanup
  , SMConnectionState(..)
  , BetId, MarketId, Handicap, SelectionId
  , StreamingMessage(..)
  , MarketDetails (..), MarketRunnerTable
  , mdMarketDefinition, mdTv, mdMarketRunners
  , MarketRunner (..), MarketRunnerKey
  , mrBackPrices, mrLayPrices, mrDispBackPrices, mrDispLayPrices, mrTv, mrLtp
  , OrderRunner(..), OrderRunnerTable
  , orMatchedBacks, orMatchedLays, orOrders
  , StreamingHandler (..)
  , getNextStreamMessage, tryGetNextStreamMessage
  , subscribeToMarkets, getMarketCache
  , subscribeToOrders, getOrderCache
  , StreamingConnectionInfo(..)
  , sciHostName, sciPort
) where

import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.Prices (LadderPrices (..),
                                                PricePoints (..))
import           Control.Lens                  (makeLenses)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.Default
import qualified Data.Map.Strict               as M
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import           Data.Time.Clock               (UTCTime)
import           Polysemy

-- | Stream message delimiter
crlf :: ByteString
crlf = B.pack [13, 10]

-- | Cache type used by market and order data caching
data StreamCache c k v = StreamCache
        { _scSubscriptionId :: !Int
        , _scClk            :: !(Maybe Text)
        , _scInitialClk     :: !(Maybe Text)
        , _scPt             :: !(Maybe Integer)
        , _scHeartbeatMs    :: !(Maybe Integer)
        , _scConflateMs     :: !(Maybe Integer)
        , _scStatus         :: !(Maybe Int)
        , _scStore          :: !(M.Map k v)
        , _scSegments       :: !(Seq.Seq [c])
        } deriving (Show)
instance Default (StreamCache c k v) where
    def = StreamCache def def def def def def def def def

makeLenses ''StreamCache

-- | Describes market and order data parser capabilities
class StreamMessageParser msg c | msg -> c where
    getMessageId        :: msg -> Maybe Int
    getCt               :: msg -> Maybe E'Ct
    getInitialClk       :: msg -> Maybe Text
    getClk              :: msg -> Maybe Text
    getHeartbeatMs      :: msg -> Maybe Integer
    getPt               :: msg -> Maybe Integer
    getConflateMs       :: msg -> Maybe Integer
    getSegmentType      :: msg -> Maybe E'SegmentType
    getStatus           :: msg -> Maybe Int
    getChanges          :: msg -> Maybe [c]

instance StreamMessageParser MarketChangeMessage MarketChange where
    getMessageId        = marketChangeMessageId
    getCt               = marketChangeMessageCt
    getInitialClk       = marketChangeMessageInitialClk
    getClk              = marketChangeMessageClk
    getHeartbeatMs      = marketChangeMessageHeartbeatMs
    getPt               = marketChangeMessagePt
    getConflateMs       = marketChangeMessageConflateMs
    getSegmentType      = marketChangeMessageSegmentType
    getStatus           = marketChangeMessageStatus
    getChanges          = marketChangeMessageMc

instance StreamMessageParser OrderChangeMessage OrderMarketChange where
    getMessageId        = orderChangeMessageId
    getCt               = orderChangeMessageCt
    getInitialClk       = orderChangeMessageInitialClk
    getClk              = orderChangeMessageClk
    getHeartbeatMs      = orderChangeMessageHeartbeatMs
    getPt               = orderChangeMessagePt
    getConflateMs       = orderChangeMessageConflateMs
    getSegmentType      = orderChangeMessageSegmentType
    getStatus           = orderChangeMessageStatus
    getChanges          = orderChangeMessageOc

-- | State used internally by streaming API
data StreamingState = StreamingState
    { _ssStreamBuffer :: !ByteString       -- ^ Temporary buffer of incoming data
    , _ssConnectionId :: !(Maybe Text)     -- ^ Connection id received initially
    , _ssAuthMsgId    :: !(Maybe Int)      -- ^ Authentication message id
    , _ssLastCleanup  :: !(Maybe UTCTime)  -- ^ Last cleanup run at
    }
instance Default StreamingState where
        def = StreamingState mempty Nothing Nothing Nothing

makeLenses ''StreamingState

type BetId = Text
type MarketId = Text
type Handicap = Maybe Double
type SelectionId = Integer

-- | Streaming API connection state
data SMConnectionState = SMCSConnected | SMCSDisconnected
    deriving (Eq, Show)

-- | Streaming update message to be received by client
data StreamingMessage = SMConnectionStateChanged SMConnectionState
                      -- ^ Connection state changed
                      | SMMarketUpdate [MarketId]
                      -- ^ Market data updated
                      | SMOrderUpdate [MarketId]
                      -- ^ Order data updated
    deriving (Show)

type MarketRunnerKey = (SelectionId, Handicap)

-- | Market runner
data MarketRunner = MarketRunner
        { _mrBackPrices     :: !LadderPrices   -- ^ Best avaialable to back
        , _mrLayPrices      :: !LadderPrices   -- ^ Best avaialable to lay
        , _mrDispBackPrices :: !LadderPrices   -- ^ Best display avaialable to back
        , _mrDispLayPrices  :: !LadderPrices   -- ^ Best display avaialable to lay
        , _mrTv             :: !(Maybe Double) -- ^ Total amount matched
        , _mrLtp            :: !(Maybe Double) -- ^ Last traded price
        } deriving (Show)

makeLenses ''MarketRunner

instance Default MarketRunner where
    def = MarketRunner def def def def def def

type MarketRunnerTable = M.Map MarketRunnerKey MarketRunner

-- | Market details
data MarketDetails = MarketDetails
        { _mdMarketDefinition :: !(Maybe MarketDefinition) -- ^ Market definition
        , _mdTv               :: !(Maybe Double)           -- ^ Total amount matched
        , _mdMarketRunners    :: !MarketRunnerTable        -- ^ Market runner table
        }

makeLenses ''MarketDetails

instance Default MarketDetails where
    def = MarketDetails def def def

-- | Order runner
data OrderRunner = OrderRunner
        { _orMatchedBacks :: !PricePoints    -- ^ Full ladder of matched backs
        , _orMatchedLays  :: !PricePoints    -- ^ Full ladder of matched lays
        , _orOrders       :: !(M.Map BetId Order)  -- ^ Order list for this runner
        } deriving (Show)

makeLenses ''OrderRunner

instance Default OrderRunner where
    def = OrderRunner def def def


type OrderRunnerTable = M.Map MarketRunnerKey OrderRunner

-- | Streaming API connection details
data StreamingConnectionInfo = StreamingConnectionInfo
        { _sciHostName :: Text          -- ^ Streaming API hostname
        , _sciPort     :: Int           -- ^ Streaming API port
        }
    deriving (Show)

makeLenses ''StreamingConnectionInfo

-- | Streaming DSL
data StreamingHandler m a where
    -- | Gets next message from queue. Blocks if there are no messages.
    GetNextStreamMessage :: StreamingHandler m StreamingMessage
    -- | Tries to get next message from queue. Returns 'Nothing', if queue is empty.
    TryGetNextStreamMessage :: StreamingHandler m (Maybe StreamingMessage)
    -- | Subscribes to market stream specified by 'MarketFilter'. There is only one
    -- one current market subscription. Following calls overwrite previous subscriptions.
    SubscribeToMarkets :: MarketFilter
                       -> StreamingHandler m ()
    -- | Retrieves market cache.
    GetMarketCache     :: StreamingHandler m (M.Map MarketId MarketDetails)
    -- | Subscribes to order stream.
    SubscribeToOrders  :: StreamingHandler m ()
    -- | Retrieves order cache.
    GetOrderCache :: StreamingHandler m (M.Map MarketId OrderRunnerTable)

makeSem_ ''StreamingHandler

-- | Gets next message from queue. Blocks if there are no messages.
getNextStreamMessage :: Member StreamingHandler r => Sem r StreamingMessage

-- | Tries to get next message from queue. Returns 'Nothing', if queue is empty.
tryGetNextStreamMessage :: Member StreamingHandler r => Sem r (Maybe StreamingMessage)

-- | Subscribes to market stream specified by 'MarketFilter'. There is only one
-- one current market subscription. Following calls overwrite previous subscriptions.
subscribeToMarkets :: Member StreamingHandler r => MarketFilter -> Sem r ()

-- | Retrieves market cache.
getMarketCache :: Member StreamingHandler r => Sem r (M.Map MarketId MarketDetails)

-- | Subscribes to order stream.
subscribeToOrders :: Member StreamingHandler r => Sem r ()

-- | Retrieves order cache.
getOrderCache :: Member StreamingHandler r => Sem r (M.Map MarketId OrderRunnerTable)
