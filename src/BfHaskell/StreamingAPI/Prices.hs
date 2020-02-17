-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.Prices
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.Prices
(
    Bet(..), BetSize, BetOdds
  , LadderPrices(..), mkLadderPrices, updateLadderPrices
  , defaultLadderLevels
  , PricePoints(..), mkPricePoints, updatePricePoints
  , liability
) where

import           Control.Monad   (guard)
import           Data.Default
import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Data.Vector     ((//))
import qualified Data.Vector     as V

type BetSize = Double
type BetOdds = Double

data Bet = Bet
    { betSize :: !BetSize
    , betOdds :: !BetOdds
    } deriving (Show, Eq)

instance Default Bet where
    def = Bet 0 0
instance Semigroup Bet where
    (Bet sa odds) <> (Bet sb _) = Bet (sa + sb) odds
instance Monoid Bet where
    mempty = def

defaultLadderLevels :: Int
defaultLadderLevels = 3

newtype LadderPrices = LadderPrices { unLadderPrices :: V.Vector Bet }
    deriving (Show)
instance Default LadderPrices where
    def = mkLadderPrices defaultLadderLevels

newtype PricePoints = PricePoints { unPricePoints :: M.Map BetOdds BetSize }
    deriving (Eq, Show)
instance Default PricePoints where
    def = mkPricePoints


mkLadderPrices :: Int -> LadderPrices
mkLadderPrices maxLadders = LadderPrices $ V.replicate maxLadders def

updateLadderPrices :: Maybe [[Double]] -> LadderPrices -> LadderPrices
updateLadderPrices Nothing oldPrices = oldPrices
updateLadderPrices (Just newPrices) (LadderPrices oldPrices) =
    LadderPrices $ oldPrices // mapMaybe toBet newPrices
  where
    ladderSize = V.length oldPrices

    toBet [level, odds, size] = do
        let l = truncate level
        guard $ l < ladderSize
        return (l, Bet size odds)
    toBet _                   = Nothing

mkPricePoints :: PricePoints
mkPricePoints = PricePoints M.empty

updatePricePoints :: Maybe [[Double]] -> PricePoints -> PricePoints
updatePricePoints Nothing old = old
updatePricePoints (Just []) _ = def
updatePricePoints (Just changes) old =
    foldl' update old changes
  where
    update (PricePoints pp) [odds, 0]    = PricePoints $ M.delete odds pp
    update (PricePoints pp) [odds, size] = PricePoints $ M.insert odds size pp
    update pp _                          = pp

liability :: Bet -> Double
liability (Bet size odds) = (odds - 1) * size
