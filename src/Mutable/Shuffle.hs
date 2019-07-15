{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions to perform shuffles on mutable vectors.
-- The shuffling is uniform amongst all permuations and uses the minimal
-- number of transpositions.

module Mutable.Shuffle where

import           Control.Monad.Primitive
import           Data.Vector.Mutable
import           Prelude                 hiding (length, tail)
import System.Random.MWC as MWC

-- |
-- Perform a shuffle on a mutable vector with a given random generator, returning a new random generator.
shuffle
  :: forall m a
  . ( PrimMonad m
    )
  => MVector (PrimState m) a -> Gen (PrimState m) -> m ()
shuffle mutV gen = go mutV (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> Int -> m ()
    go _ 0   =  pure ()
    go v maxInd =
      do
        ind :: Int <- MWC.uniformR (0, maxInd) gen
        swap v 0 ind
        go (tail v) (maxInd - 1)

shuffleK
  :: forall m a
  . ( PrimMonad m
    )
  => Int -> MVector (PrimState m) a -> Gen (PrimState m) -> m ()
shuffleK numberOfShuffles mutV gen = go mutV gen (numberOfShuffles - 1)
  where
    go :: MVector (PrimState m) a -> Gen (PrimState m) -> Int -> m ()
    go _ _ k | k < 0
      = error "The argument to shuffleK must be a positive value."
    go _ _ k | k > (length mutV)
      = error "The argument to shuffleK must not exceed the length of the vector"
    go _ _ 0   =  pure ()
    go v g maxInd =
      do
        ind :: Int <- MWC.uniformR (0, maxInd) g
        swap v 0 ind
        go (tail v) g (maxInd - 1)

