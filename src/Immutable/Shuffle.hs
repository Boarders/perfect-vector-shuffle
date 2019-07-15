{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | This module provides functions to perform shuffles on immutable vectors.
-- The shuffling is uniform amongst all permuations and performs the minimal
-- number of transpositions.

module Immutable.Shuffle where

import           Control.Monad.Primitive
import           Data.Vector
import qualified Mutable.Shuffle         as MS
import           Prelude                 hiding (length)
import System.Random.MWC as MWC


-- |
-- Perform a shuffle on an immutable vector with a given random generator returning a shuffled vector and a new generator.
shuffle :: forall a m . PrimMonad m => Gen (PrimState m) -> Vector a -> m (Vector a)
shuffle g v
  | length v <= 1 = pure v
  | otherwise =
        do
          mutV   <- thaw v
          MS.shuffle mutV g
          immutV <- unsafeFreeze mutV
          pure immutV

-- |
-- Perform a shuffle on an immutable vector with a given random generator returning a shuffled vector and a new generator.
shuffleK :: forall a m . PrimMonad m => Int -> Gen (PrimState m) -> Vector a -> m (Vector a)
shuffleK k g v
  | length v <= 1 = pure v
  | otherwise =
        do
          mutV   <- thaw v
          MS.shuffleK k mutV g
          immutV <- unsafeFreeze mutV
          pure immutV


-- |
-- Perform a shuffle on an input immutable vector in a monad which has a source of randomness.
shuffleIO :: Vector a -> IO (Vector a)
shuffleIO v =
  do
    gen <- createSystemRandom
    shuffle gen v


