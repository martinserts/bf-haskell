-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.StreamingOrders
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.StreamingOrders
(
    extractOrderChanges
  , OrderCache
) where

import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.Prices (updatePricePoints)
import           BfHaskell.StreamingAPI.Types  (BetId, MarketId, OrderRunner,
                                                OrderRunnerTable,
                                                StreamCache (..),
                                                orMatchedBacks, orMatchedLays,
                                                orOrders, scStore)
import           Control.Lens                  (over, (%~), (&))
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe
import           Data.Default
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe)
import           Polysemy
import           Polysemy.State

type OrderCache = StreamCache OrderMarketChange MarketId OrderRunnerTable


-- | Updates cache with OrderMarketChange
-- Returns corresponding market id if operation was successful
extractOrderChanges :: Member (State OrderCache) r
                    => OrderMarketChange
                    -> Sem r (Maybe MarketId)
extractOrderChanges oc =
    runMaybeT $ do
        marketId <- MaybeT . pure $ orderMarketChangeId oc
        lift . modify . over scStore $ \store ->
            -- Remove market if it is closed
            if isClosed then M.delete marketId store
            else
              case orderMarketChangeOrc oc of
                Nothing -> store
                Just rcs ->
                  -- Fetch market details from store or create new one if it's not found
                  let currentRunnerTable = fromMaybe M.empty $ M.lookup marketId store
                      updatedRunnerTable = foldl updateMarket currentRunnerTable rcs
                  -- Update market details in store
                  in M.insert marketId updatedRunnerTable store
        return marketId
  where
    isClosed = orderMarketChangeClosed oc == Just True

-- | Updates runner table with runner change
updateMarket :: OrderRunnerTable -> OrderRunnerChange -> OrderRunnerTable
updateMarket t rc =
    case runnerKey of
      Nothing -> t
      Just key ->
          let currentRunner = if isImage then Nothing else M.lookup key t
              updatedRunner = updateRunner rc $ fromMaybe def currentRunner
          in M.insert key updatedRunner t
  where
    runnerKey = do
        selectionId <- orderRunnerChangeId rc
        let handicap = orderRunnerChangeHc rc
        return (selectionId, handicap)

    isImage = orderRunnerChangeFullImage rc == Just True

-- | Updates runner with runner change
updateRunner :: OrderRunnerChange -> OrderRunner -> OrderRunner
updateRunner rc orderRunner =
    orderRunner & orMatchedBacks %~ updatePricePoints (orderRunnerChangeMb rc)
                & orMatchedLays %~ updatePricePoints (orderRunnerChangeMl rc)
                & orOrders %~ updateOrders (orderRunnerChangeUo rc)

-- | Adds list of orders to order table
updateOrders :: Maybe [Order] -> M.Map BetId Order -> M.Map BetId Order
updateOrders Nothing orderTable       = orderTable
updateOrders (Just orders) orderTable = foldl addOrder orderTable orders

-- | Adds and order to order table
addOrder :: M.Map BetId Order -> Order -> M.Map BetId Order
addOrder m order = fromMaybe m add
  where
    add = do
        oid <- orderId order
        return $ M.insert oid order m
