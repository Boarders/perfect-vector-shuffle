{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mutable.Shuffle where

import           Control.Monad.Primitive
import           Control.Monad.Random    (MonadRandom (..))
import           Data.Vector.Mutable
import           Prelude                 hiding (length, tail)
import           System.Random           (RandomGen)
import qualified System.Random           as SR

-- |
-- Perform a perfect shuffle on an input mutable vector v with a given random generator.
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
    go v g len =
      do
        let (ind, newGen) :: (Int, g) = SR.randomR (0, len) g
        swap v 0 ind
        go (tail v) newGen (len - 1)


-- |
-- Perform a perfect shuffle on an input mutable vector in a monad which has a source of randomness.
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
    go v len =
      do
        ind <-  getRandomR (0, len)
        swap v 0 ind
        go (tail v) (len - 1)


