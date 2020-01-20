-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.StreamingMarkets
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.StreamingMarkets
(
    extractMarketChanges
  , cleanupMarket
  , MarketCache
) where

import           BfHaskell.Common.Odds                 (OddsTree)
import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.Prices         (updateLadderPrices)
import           BfHaskell.StreamingAPI.StreamingUtils (updateStreamingProperty)
import           BfHaskell.StreamingAPI.Types          (MarketDetails (..),
                                                        MarketId,
                                                        MarketRunner (..),
                                                        MarketRunnerTable,
                                                        StreamCache,
                                                        mdMarketDefinition,
                                                        mdMarketRunners, mdTv,
                                                        mrBackPrices,
                                                        mrDispBackPrices,
                                                        mrDispLayPrices,
                                                        mrLayPrices, mrLtp,
                                                        mrTv, scStore)
import           BfHaskell.StreamingAPI.VirtualPrices  (displayPrices)
import           Control.Lens
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Maybe
import           Data.Default
import qualified Data.Map                              as M
import           Data.Maybe                            (fromMaybe)
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State


type MarketCache = StreamCache MarketChange MarketId MarketDetails


-- | Updates cache with MarketChange
-- Returns corresponding market id if operation was successful
extractMarketChanges :: Members '[State MarketCache, Reader OddsTree] r
                     => MarketChange
                     -> Sem r (Maybe MarketId)
extractMarketChanges mc = do
    oddsTree <- ask
    runMaybeT $ do
        marketId <- MaybeT . pure $ marketChangeId mc
        lift . modify . over scStore $ \store ->
            let currentMarketDetails = fromMaybe def $ M.lookup marketId store
                -- Fetch market details from store or create new one if it's not found
                updatedMarketDetails = updateMarket oddsTree currentMarketDetails mc
                -- Update market details in store
                in M.insert marketId updatedMarketDetails store
        return marketId

-- | Updates market with MarketChange
updateMarket :: OddsTree -> MarketDetails -> MarketChange -> MarketDetails
updateMarket ot md mc =
    md & mdMarketDefinition `updateStreamingProperty` marketChangeMarketDefinition mc
       & mdTv `updateStreamingProperty` marketChangeTv mc
       & mdMarketRunners %~ \mr ->
            case marketChangeRc mc of
                Nothing -> mr
                Just rcs -> do
                    let isImage = marketChangeImg mc == Just True
                    foldl (processRunner isImage) mr rcs
       & updateDisplayPrices ot md

-- | Get corresponding runner from cache or create a new one
-- Then update with changes in RunnerChange
-- Save the result back to cache
processRunner :: Bool
              -> MarketRunnerTable
              -> RunnerChange
              -> MarketRunnerTable
processRunner isImage mrt rc =
    case runnerKey of
      Nothing -> mrt
      Just key ->
          let currentRunner = if isImage then Nothing else M.lookup key mrt
              updatedRunner = updateRunner rc $ fromMaybe def currentRunner
          in M.insert key updatedRunner mrt
  where
    runnerKey = do
        selectionId <- runnerChangeId rc
        let handicap = runnerChangeHc rc
        return (selectionId, handicap)

-- Updates display prices for all runners in this market
updateDisplayPrices :: OddsTree
                    -> MarketDetails
                    -> MarketRunnerTable
                    -> MarketRunnerTable
updateDisplayPrices ot md mrt =
    foldl applyDisplayPrices mrt $ M.toList mrt
  where
    applyDisplayPrices mrt' (key@(sid, hc), mr) =
        let (back, lay) = displayPrices ot md sid hc mr
            mr' = mr & mrDispBackPrices .~ back
                     & mrDispLayPrices .~ lay
        in M.insert key mr' mrt'

-- | Updates MarketRunner with RunnerChange
updateRunner :: RunnerChange -> MarketRunner -> MarketRunner
updateRunner rc mr =
    mr & mrTv `updateStreamingProperty` runnerChangeTv rc
       & mrLtp `updateStreamingProperty` runnerChangeLtp rc
       & mrBackPrices %~ updateLadderPrices (runnerChangeBatb rc)
       & mrLayPrices  %~ updateLadderPrices (runnerChangeBatl rc)

-- | Cleanup market cache
cleanupMarket :: Member (State MarketCache) r => Sem r ()
cleanupMarket = modify . over scStore $ \store ->
    M.filter (not . isClosed) store     -- Remove closed markets
  where
    isClosed = has $ mdMarketDefinition . _Just . to marketDefinitionStatus
                                        . _Just . only E'Status'CLOSED
