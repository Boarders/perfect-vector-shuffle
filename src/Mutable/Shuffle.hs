{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions to perform shuffles on mutable vectors.
-- The shuffling is uniform amongst all permuations and uses the minimal
-- number of transpositions.

module Mutable.Shuffle where

import           Control.Monad.Primitive
import           Control.Monad.Random    (MonadRandom (..))
import           Data.Vector.Mutable
import           Prelude                 hiding (length, tail)
import           System.Random           (RandomGen)
import qualified System.Random           as SR

-- |
-- Perform a shuffle on a mutable vector with a given random generator, returning a new random generator.
shuffle
  :: forall m a g
  . ( PrimMonad m
    , RandomGen g
    )
  => MVector (PrimState m) a -> g -> m g
shuffle mutV gen = go mutV gen (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> g -> Int -> m g
    go _ g 0   =  pure g
    go v g maxInd =
      do
        let (ind, newGen) :: (Int, g) = SR.randomR (0, maxInd) g
        swap v 0 ind
        go (tail v) newGen (maxInd - 1)


-- |
-- Perform a shuffle on a mutable vector in a monad which has a source of randomness.
shuffleM
  :: forall m a
  . ( PrimMonad m
    , MonadRandom m
    )
  => MVector (PrimState m) a  -> m ()
shuffleM mutV = go mutV (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> Int -> m ()
    go _ 0   =  pure ()
    go v maxInd =
      do
        ind <-  getRandomR (0, maxInd)
        swap v 0 ind
        go (tail v) (maxInd - 1)


