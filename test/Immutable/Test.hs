{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Immutable.Test
  ( testSuite
  ) where

import           Data.List                        (sort)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           Immutable.Shuffle
import           System.Random
import           Test.QuickCheck.Instances.Vector ()
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck            as QC hiding (shuffle)


testSuite :: TestTree
testSuite = testGroup ""
  [ localOption (QuickCheckTests 1000) shuffleTestSuite
  , localOption (QuickCheckTests 1000) shuffleMTestSuite
  ]


shuffleTestSuite :: TestTree
shuffleTestSuite = testGroup "shuffle"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . isPermutation @Int)]


shuffleMTestSuite :: TestTree
shuffleMTestSuite = testGroup "shuffleM"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . isPermutationM @Int)]


isPermutation :: forall a . (Ord a , Show a, Arbitrary a) => Vector a -> PropertyM IO Property
isPermutation v =
  do
    gen <- run $ getStdGen
    let v' = fst $ shuffle v gen
    let ls  = V.toList v
    let ls' = V.toList v'
    pure $ (sort ls) === (sort ls')


isPermutationM :: forall a . (Ord a , Show a, Arbitrary a) => Vector a -> PropertyM IO Property
isPermutationM v =
  do
    v'  <- run $ shuffleM v
    let ls  = V.toList v
    let ls' = V.toList v'
    pure $ (sort ls) === (sort ls')

