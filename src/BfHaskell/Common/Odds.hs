-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.Common.Odds
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BfHaskell.Common.Odds
(
    Odds
  , OddsTree
  , NormalizedOdds(..)
  , normalizeOdds
  , nextOdds
  , newTree
) where

import           Data.Fixed                  (Nano, mod')
import           Data.IntervalMap.FingerTree (Interval (..), IntervalMap, empty,
                                              insert, search)

-- | Betting odds
type Odds = Nano
type OddsStep = Nano

-- | Odds normalized for Betfair exchange
-- See: [Betfair Price Increments](https://docs.developer.betfair.com/display/1smk3cen4v3lu3yomq5qye0ni/placeOrders#placeOrders-BetfairPriceIncrements)
newtype NormalizedOdds = NormalizedOdds { getOdds :: Nano }
        deriving (Eq, Show, Num, Ord)

type OddsTree = IntervalMap Odds OddsStep

-- | Betfair odds normalization tree
newTree :: OddsTree
newTree = foldl insertInterval empty intervals
  where
    intervals = [(0, 2, 0.01),
                 (2, 3, 0.02),
                 (3, 4, 0.05),
                 (4, 6, 0.1),
                 (6, 10, 0.2),
                 (10, 20, 0.5),
                 (20, 30, 1),
                 (30, 50, 2),
                 (50, 100, 5),
                 (100, 100000, 10)]
    insertInterval m (from, till, step) = insert (Interval from (till - step)) step m

getOddsStep :: OddsTree -> Odds -> OddsStep
getOddsStep tree odds =
    case search odds tree of
      (_, step):_ -> step
      _           -> 0

-- | Normalize odds
normalizeOdds :: OddsTree   -- ^ Normalization tree
              -> Odds       -- ^ Odds
              -> Bool       -- ^ 'True' round up. 'False' - round down.
              -> NormalizedOdds
normalizeOdds tree odds roundUp
    | roundUp && remainder > 0 = NormalizedOdds $ rounded + oddsStep
    | otherwise = NormalizedOdds rounded
  where
    oddsStep = getOddsStep tree odds
    remainder = odds `mod'` oddsStep
    rounded = odds - remainder

-- | Returns next normalized odds using price increments
nextOdds :: OddsTree -> NormalizedOdds -> NormalizedOdds
nextOdds tree odds = odds + NormalizedOdds (getOddsStep tree (getOdds odds))
