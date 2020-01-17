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
  , MarketCache
) where

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
import           Control.Lens                          (over, (%~), (&))
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Maybe
import           Data.Default
import qualified Data.Map                              as M
import           Data.Maybe                            (fromMaybe)
import           Polysemy
import           Polysemy.State


type MarketCache = StreamCache MarketChange MarketId MarketDetails


-- | Updates cache with MarketChange
-- Returns corresponding market id if operation was successful
extractMarketChanges :: Member (State MarketCache) r
                     => MarketChange
                     -> Sem r (Maybe MarketId)
extractMarketChanges mc =
    runMaybeT $ do
        marketId <- MaybeT . pure $ marketChangeId mc
        lift . modify . over scStore $ \store ->
            -- Remove market if it is closed
            if marketStatus == Just E'Status'CLOSED then M.delete marketId store
            else
                -- Fetch market details from store or create new one if it's not found
                let currentMarketDetails = fromMaybe def $ M.lookup marketId store
                    updatedMarketDetails = updateMarket currentMarketDetails mc
                -- Update market details in store
                in M.insert marketId updatedMarketDetails store
        return marketId
  where
    marketStatus = marketChangeMarketDefinition mc >>= marketDefinitionStatus

-- | Updates market with MarketChange
updateMarket :: MarketDetails -> MarketChange -> MarketDetails
updateMarket md mc =
    md & mdMarketDefinition `updateStreamingProperty` marketChangeMarketDefinition mc
       & mdTv `updateStreamingProperty` marketChangeTv mc
       & mdMarketRunners %~ \mr ->
            case marketChangeRc mc of
                Nothing -> mr
                Just rcs -> do
                    let isImage = marketChangeImg mc == Just True
                    foldl (processRunner isImage) mr rcs

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

-- | Updates MarketRunner with RunnerChange
updateRunner :: RunnerChange -> MarketRunner -> MarketRunner
updateRunner rc mr =
    mr & mrTv `updateStreamingProperty` runnerChangeTv rc
       & mrLtp `updateStreamingProperty` runnerChangeLtp rc
       & mrBackPrices %~ updateLadderPrices (runnerChangeBatb rc)
       & mrLayPrices  %~ updateLadderPrices (runnerChangeBatl rc)
       & mrDispBackPrices %~ updateLadderPrices (runnerChangeBdatb rc)
       & mrDispLayPrices  %~ updateLadderPrices (runnerChangeBdatl rc)
