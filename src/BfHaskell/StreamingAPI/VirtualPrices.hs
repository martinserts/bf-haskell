-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.VirtualPrices
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.VirtualPrices
(
    displayPrices
) where

import           BfHaskell.Common.Odds
import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.Prices
import           BfHaskell.StreamingAPI.Types
import           Control.Lens
import           Control.Monad                 (guard, join)
import           Data.Default
import           Data.List                     (groupBy)
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.Ord                      (Down (..))
import qualified Data.Vector                   as V
import           GHC.Exts                      (sortWith)


virtualPrices :: OddsTree
              -> MarketDetails
              -> SelectionId
              -> Handicap
              -> (MarketRunner -> LadderPrices)
              -> Bool
              -> Maybe [Bet]
virtualPrices ot md sid hc extractPrices roundUp =
    mapMaybe virtualBet . mapM (V.toList . unLadderPrices) <$> otherPrices
  where
    otherSelectionIds = do
        runners <- marketDefinitionRunners =<< md ^. mdMarketDefinition
        return $ mapMaybe runnerDefinitionId $
                    filter (\r -> runnerDefinitionHc r == hc &&
                                  runnerDefinitionId r /= Just sid)
                           runners

    findPrices selId = md ^? mdMarketRunners . ix (selId, hc) . to extractPrices

    otherPrices = mapM findPrices =<< otherSelectionIds

    virtualBet bets = do
        guard $ odds > 0
        return $ Bet (fromIntegral size) nOdds
      where
        odds = 1.0 / (1.0 - sum (map (\b -> 1.0 / betOdds b) bets))
        nOdds = fromRational . toRational . getOdds $
                    normalizeOdds ot (realToFrac odds) roundUp
        size :: Integer
        size = floor $ minimum (map (\b -> betSize b * betOdds b) bets) / nOdds

displayPrices :: OddsTree
              -> MarketDetails
              -> SelectionId
              -> Handicap
              -> MarketRunner
              -> (LadderPrices, LadderPrices)        -- Back and Lay
displayPrices ot md sid hc mr =
    (toPrices backBets, toPrices layBets)
  where
    toPrices = LadderPrices . V.fromList

    toMergedPrices getOtherPrices roundUp getRealPrices descending =
        dropDups $ sortOdds $ realPrices ++ fromMaybe [] mVirtualPrices
      where
        mVirtualPrices = virtualPrices ot md sid hc getOtherPrices roundUp
        realPrices = filter (/= def) . V.toList . unLadderPrices $ getRealPrices mr
        sortOdds = if descending then sortWith (Down . betOdds)
                                 else sortWith betOdds
        dropDups bets = join $ map (pure . mconcat)
                             $ groupBy (\a b -> betOdds a == betOdds b) bets

    backBets = toMergedPrices _mrLayPrices False _mrBackPrices True
    layBets = toMergedPrices _mrBackPrices True _mrLayPrices False
