{-# LANGUAGE ScopedTypeVariables #-}
module Immutable.Shuffle where

import           Control.Monad.Primitive
import           Control.Monad.Random    (MonadRandom (..))
import           Control.Monad.ST        (runST)
import           Data.Vector
import qualified Mutable.Shuffle         as MS
import           Prelude                 hiding (length)
import           System.Random           (RandomGen (..))


-- |
-- Perform a perfect shuffle on an input immutable vector v with a given random generator.
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
-- Perform a perfect shuffle on an input immutable vector in a monad which has a source of randomness.
shuffleM :: forall m a . (MonadRandom m, PrimMonad m) => Vector a -> m (Vector a)
shuffleM v
  | length v <= 1 = pure v
  | otherwise =
      do
        mutV   <- thaw v
        MS.shuffleM mutV
        unsafeFreeze mutV
