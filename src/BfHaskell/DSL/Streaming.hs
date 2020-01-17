-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.DSL.Streaming
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.DSL.Streaming
(
  -- * Effect
  StreamingHandler(..)

  -- * Actions
  , getNextStreamMessage
  , tryGetNextStreamMessage
  , subscribeToMarkets
  , getMarketCache
  , subscribeToOrders
  , getOrderCache

  -- * Interpretations
  , runStreamingHandler

  -- * Helpers
  , defaultStreamingConnectionInfo
  , mkMarketListFilter

  -- * Data types
  , Handicap, MarketDetails, MarketId, MarketRunner(..), MarketRunnerKey
  , SMConnectionState (..), SelectionId, StreamingConnectionInfo (..)
  , StreamingMessage (..), OrderRunner(..), OrderRunnerTable

  -- * Lens
  , mdMarketDefinition, mdTv, mdMarketRunners
  , mrBackPrices, mrLayPrices, mrDispBackPrices, mrDispLayPrices, mrTv, mrLtp
  , orMatchedBacks, orMatchedLays, orOrders

) where

import           BfHaskell.StreamingAPI.SslClient          (defaultStreamingConnectionInfo)
import           BfHaskell.StreamingAPI.Streaming          (mkMarketListFilter)
import           BfHaskell.StreamingAPI.StreamingProcessor (runStreamingHandler)
import           BfHaskell.StreamingAPI.Types              (Handicap,
                                                            MarketDetails,
                                                            MarketId,
                                                            MarketRunner (..),
                                                            MarketRunnerKey,
                                                            OrderRunner (..),
                                                            OrderRunnerTable,
                                                            SMConnectionState (..),
                                                            SelectionId,
                                                            StreamingConnectionInfo (..),
                                                            StreamingHandler (..),
                                                            StreamingMessage (..),
                                                            getMarketCache,
                                                            getNextStreamMessage,
                                                            getOrderCache,
                                                            mdMarketDefinition,
                                                            mdMarketRunners,
                                                            mdTv, mrBackPrices,
                                                            mrDispBackPrices,
                                                            mrDispLayPrices,
                                                            mrLayPrices, mrLtp,
                                                            mrTv,
                                                            orMatchedBacks,
                                                            orMatchedLays,
                                                            orOrders,
                                                            subscribeToMarkets,
                                                            subscribeToOrders,
                                                            tryGetNextStreamMessage)
