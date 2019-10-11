{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions to perform shuffles on mutable vectors.
-- The shuffling is uniform amongst all permuations and uses the minimal
-- number of transpositions.

module Mutable.Shuffle where

import           Control.Monad.Primitive
import           Control.Monad.Random    (MonadRandom (..))
import           Data.Vector.Mutable
import           Prelude                 hiding (length, read, tail)
import           System.Random           (RandomGen)
import qualified System.Random           as SR


-- |
-- Perform a shuffle on a mutable vector with a given random generator, returning a new random generator.
--
-- This uses the Fisher--Yates--Knuth algorithm
shuffle
  :: forall m a g
  . ( PrimMonad m
    , RandomGen g
    )
  => MVector (PrimState m) a -> g -> m g
{-# INLINABLE shuffle #-}
shuffle mutV gen = go mutV gen (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> g -> Int -> m g
    {-# INLINE go #-}
    go _ g 0   =  pure g
    go v g maxInd =
      do
        let (ind, newGen) :: (Int, g) = SR.randomR (0, maxInd) g
        swap v 0 ind
        go (tail v) newGen (maxInd - 1)



-- |
-- Perform a shuffle on a mutable vector in a monad which has a source of randomness.
--
-- This uses the Fisher--Yates--Knuth algorithm
shuffleM
  :: forall m a
  . ( PrimMonad m
    , MonadRandom m
    )
  => MVector (PrimState m) a  -> m ()
{-# INLINABLE shuffleM #-}
shuffleM mutV = go mutV (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> Int -> m ()
    {-# INLINE go #-}
    go _ 0   =  pure ()
    go v maxInd =
      do
        ind <-  getRandomR (0, maxInd)
        swap v 0 ind
        go (tail v) (maxInd - 1)

{-# SPECIALISE shuffleM :: MVector RealWorld a -> IO () #-}

-- |
-- Shuffle the first k elements of a vector.
--
shuffleK
  :: forall m a
  . ( PrimMonad m
    , MonadRandom m
    )
  => Int -> MVector (PrimState m) a  -> m ()
{-# INLINABLE shuffleK #-}
shuffleK numberOfShuffles mutV = go mutV (numberOfShuffles - 1)
  where
    go :: MVector (PrimState m) a -> Int -> m ()
    {-# INLINE go #-}
    go _ k | k < 0
      = error "Cannot pass negative value to ShuffleK"
    go _ k | k >= length mutV
      = error "Cannot pass value greater than the length of the vector  to ShuffleK"
    go _ 0   =  pure ()
    go v maxInd =
      do
        ind <-  getRandomR (0, maxInd)
        swap v 0 ind
        go (tail v) (maxInd - 1)



-- |
-- Perform a shuffle on a mutable vector wherein the shuffled indices form a maximal cycle.
--
-- This uses the Sattolo algorithm.
maximalCycle
  :: forall m a g
  . ( PrimMonad m
    , RandomGen g
    )
  => MVector (PrimState m) a -> g -> m g
{-# INLINABLE maximalCycle #-}
maximalCycle mutV gen = go mutV gen (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> g -> Int -> m g
    {-# INLINE go #-}
    go _ g 0      =  pure g
    go v g maxInd =
      do
        let (ind, newGen) :: (Int, g) = SR.randomR (1, maxInd) g
        swap v 0 ind
        go (tail v) newGen (maxInd - 1)


-- |
-- Perform a shuffle on a mutable vector wherein the shuffled indices form a maximal cycle
-- in a monad with a source of randomness.
--
-- This uses the Sattolo algorithm.
maximalCycleM
  :: forall m a
  . ( PrimMonad m
    , MonadRandom m
    )
  => MVector (PrimState m) a  -> m ()
{-# INLINABLE maximalCycleM #-}
maximalCycleM mutV = go mutV (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> Int -> m ()
    {-# INLINE go #-}
    go _ 0   =  pure ()
    go v maxInd =
      do
        ind <-  getRandomR (1, maxInd)
        swap v 0 ind
        go (tail v) (maxInd - 1)

{-# SPECIALISE maximalCycleM :: MVector RealWorld a -> IO () #-}



-- |
-- Perform a [derangement](https://en.wikipedia.org/wiki/Derangement) on a mutable vector with a given random generator, returning a new random generator.
--
-- __Note:__ It is assumed the input vector consists of distinct values.
--
-- This uses the "early refusal" algorithm.
derangement
  :: forall m a g
  . ( PrimMonad m
    , RandomGen g
    , Eq a
    )
  => MVector (PrimState m) a -> g -> m g
{-# INLINABLE derangement #-}
derangement mutV gen = do
  mutV_copy <- clone mutV
  go mutV_copy mutV gen 0 (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> MVector (PrimState m) a -> g -> Int -> Int -> m g
    {-# INLINE go #-}
    go c v g lastInd 0 =
      do
        v_last_old <- read c lastInd
        v_last_new <- read v 0
        if v_last_old == v_last_new then
          do
            unsafeCopy mutV c
            go c mutV g 0 (length mutV - 1)
        else
          pure g
    go c v oldGen currInd maxInd =
      do
        let (swapInd, newGen) :: (Int, g) = SR.randomR (0, maxInd) oldGen
        v_old  <- read c currInd
        v_ind  <- read v swapInd
        if v_old == v_ind then
          do
            unsafeCopy mutV c
            go c mutV newGen 0 (length mutV - 1)
        else
          do
            swap v 0 swapInd
            go c (tail v) newGen (currInd + 1) (maxInd - 1)


-- |
-- Perform a [derangement](https://en.wikipedia.org/wiki/Derangement) on a mutable vector in a monad which has a source of randomness.
--
-- __Note:__ It is assumed the input vector consists of distinct values.
--
-- This uses the "early refusal" algorithm
derangementM
  :: forall m a
  . ( PrimMonad m
    , MonadRandom m
    , Eq a
    )
  => MVector (PrimState m) a -> m ()
{-# INLINABLE derangementM #-}
derangementM mutV = do
  mutV_copy <- clone mutV
  go mutV_copy mutV 0 (length mutV - 1)
  where
    go :: MVector (PrimState m) a -> MVector (PrimState m) a -> Int -> Int -> m ()
    {-# INLINE go #-}
    go c v lastInd 0 =
      do
        v_last_old <- read c lastInd
        v_last_new <- read v 0
        if v_last_old == v_last_new then
          do
            unsafeCopy mutV c
            go c mutV 0 (length mutV - 1)
        else
          pure ()
    go c v currInd maxInd =
      do
        swapInd :: Int <- getRandomR (0, maxInd)
        v_old  <- read c currInd
        v_ind  <- read v swapInd
        if v_old == v_ind then
          do
            unsafeCopy mutV c
            go c mutV 0 (length mutV - 1)
        else
          do
            swap v 0 swapInd
            go c (tail v) (currInd + 1) (maxInd - 1)
