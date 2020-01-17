-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.StreamingAPI.Streaming
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

module BfHaskell.StreamingAPI.Streaming
(
    mkMarketListFilter
) where

import           BfHaskell.StreamingAPI.Model
import           BfHaskell.StreamingAPI.Types (MarketId)

-- | Helper to create market filter containing list for market ids
mkMarketListFilter :: [MarketId] -> MarketFilter
mkMarketListFilter marketIds =
    mkMarketFilter { marketFilterTurnInPlayEnabled = Just True
                   , marketFilterMarketIds = Just marketIds
                   }
