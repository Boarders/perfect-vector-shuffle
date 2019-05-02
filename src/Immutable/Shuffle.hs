{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions to perform shuffles on immutable vectors.
-- The shuffling is uniform amongst all permuations and performs the minimal
-- number of transpositions.

module Immutable.Shuffle where

import           Control.Monad.Primitive
import           Control.Monad.Random    (MonadRandom (..))
import           Control.Monad.ST        (runST)
import           Data.Vector
import qualified Mutable.Shuffle         as MS
import           Prelude                 hiding (length)
import           System.Random           (RandomGen (..))


-- |
-- Perform a shuffle on an immutable vector with a given random generator returning a shuffled vector and a new generator.
shuffle :: forall a g. RandomGen g => Vector a -> g -> (Vector a, g)
shuffle v g
  | length v <= 1 = (v, snd . next $ g)
  | otherwise     =
      runST $
        do
          mutV   <- thaw v
          newGen <- MS.shuffle mutV g
          immutV <- unsafeFreeze mutV
          pure (immutV, newGen)


-- |
-- Perform a shuffle on an input immutable vector in a monad which has a source of randomness.
shuffleM :: forall m a . (MonadRandom m, PrimMonad m) => Vector a -> m (Vector a)
shuffleM v
  | length v <= 1 = pure v
  | otherwise =
      do
        mutV   <- thaw v
        MS.shuffleM mutV
        unsafeFreeze mutV
