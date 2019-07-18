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
import           Prelude                 hiding (length, take)
import           System.Random           (RandomGen (..))


-- |
-- Perform a shuffle on an immutable vector with a given random generator returning a shuffled vector and a new generator.
--
-- This uses the Fisher--Yates--Knuth algorithm.
shuffle :: forall a g. RandomGen g => Vector a -> g -> (Vector a, g)
shuffle v g
  | length v <= 1 = (v, g)
  | otherwise     =
      runST $
        do
          mutV   <- thaw v
          newGen <- MS.shuffle mutV g
          immutV <- unsafeFreeze mutV
          pure (immutV, newGen)


-- |
-- Perform a shuffle on an input immutable vector in a monad which has a source of randomness.
--
-- This uses the Fisher--Yates--Knuth algorithm.
shuffleM :: forall m a . (MonadRandom m, PrimMonad m) => Vector a -> m (Vector a)
shuffleM v
  | length v <= 1 = pure v
  | otherwise =
      do
        mutV   <- thaw v
        MS.shuffleM mutV
        unsafeFreeze mutV


-- |
-- Perform a shuffle on the first k elements of a vector in a monad which has a
-- source of randomness.
--
shuffleK :: forall m a . (MonadRandom m, PrimMonad m) => Int -> Vector a -> m (Vector a)
shuffleK k v
  | length v <= 1 = pure v
  | otherwise =
      do
        mutV   <- thaw v
        MS.shuffleK k mutV
        unsafeFreeze mutV


-- |
-- Get a random sample of k elements without replacement from a vector.
sampleWithoutReplacement :: forall m a . (MonadRandom m, PrimMonad m) => Int -> Vector a -> m (Vector a)
{-# INLINEABLE sampleWithoutReplacement #-}
sampleWithoutReplacement k v = take k <$> (shuffleK k v)


-- |
-- Perform an in-place shuffle on an immutable vector wherein the shuffled
-- indices form a maximal cycle.
--
-- This uses the Sattolo algorithm.
maximalCycle :: forall a g. RandomGen g => Vector a -> g -> (Vector a, g)
maximalCycle v g
  | length v <= 1 = (v, g)
  | otherwise     =
      runST $
        do
          mutV   <- thaw v
          newGen <- MS.maximalCycle mutV g
          immutV <- unsafeFreeze mutV
          pure (immutV, newGen)

-- |
-- Perform an in-place shuffle on an immutable vector wherein the shuffled
-- indices form a maximal cycle in a monad with a source of randomness.
--
-- This uses the Sattolo algorithm.
maximalCycleM :: forall m a . (MonadRandom m, PrimMonad m) => Vector a -> m (Vector a)
maximalCycleM v
  | length v <= 1 = pure v
  | otherwise =
      do
        mutV   <- thaw v
        MS.maximalCycleM mutV
        unsafeFreeze mutV


-- |
-- Perform an in-place  [derangement](https://en.wikipedia.org/wiki/Derangement)
-- on an immutable vector with a given random generator, returning a new random
-- generator.
--
-- Note: It is assumed the input vector consists of distinct values.
--
-- This uses the "early refusal" algorithm.
derangement :: forall a g. (Eq a, RandomGen g) => Vector a -> g -> (Vector a, g)
derangement v g
  | length v <= 1 = (v, g)
  | otherwise     =
      runST $
        do
          mutV   <- thaw v
          newGen <- MS.derangement mutV g
          immutV <- unsafeFreeze mutV
          pure (immutV, newGen)


-- |
-- Perform an in-place [derangement](https://en.wikipedia.org/wiki/Derangement) on
-- an immutable vector in a monad which has a source of randomness.
--
-- This uses the "early refusal" algorithm.
derangementM :: forall m a . (Eq a, MonadRandom m, PrimMonad m) => Vector a -> m (Vector a)
derangementM v
  | length v <= 1 = pure v
  | otherwise =
      do
        mutV   <- thaw v
        MS.derangementM mutV
        unsafeFreeze mutV
