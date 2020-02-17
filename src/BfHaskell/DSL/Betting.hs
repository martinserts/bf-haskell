-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.DSL.Betting
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.DSL.Betting
(
  -- * Effect
  BettingHandler(..)

  -- * Actions
  , listEventTypes, listTimeRanges, listCompetitions, listMarketCatalogue
  , listMarkets, listMarketBook
  , placeOrders
  , cancelOrders, cancelMarketOrders, cancelAllOrders
  , replaceOrders

 -- * Interpretations
  , runBettingHandler

 -- * Helpers
  , defaultBettingUrl
  , createMarketFilter

  -- * Data types
  , module BfHaskell.BettingAPI.Types
  , module BfHaskell.BettingAPI.Request
  , module BfHaskell.BettingAPI.Response
) where

import           BfHaskell.BettingAPI.Betting  (BettingHandler (..),
                                                cancelAllOrders,
                                                cancelMarketOrders,
                                                cancelOrders,
                                                createMarketFilter,
                                                defaultBettingUrl,
                                                listCompetitions,
                                                listEventTypes, listMarketBook,
                                                listMarketCatalogue,
                                                listMarkets, listTimeRanges,
                                                placeOrders, replaceOrders,
                                                runBettingHandler)
import           BfHaskell.BettingAPI.Request
import           BfHaskell.BettingAPI.Response
import           BfHaskell.BettingAPI.Types
