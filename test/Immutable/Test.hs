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
  [ localOption (QuickCheckTests 10000) shuffleIOTestSuite
  , localOption (QuickCheckTests 10   ) performanceTest
  ]

performanceTest :: TestTree
performanceTest = testGroup "Performance"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . sameLength)]

shuffleIOTestSuite :: TestTree
shuffleIOTestSuite = testGroup "shuffleIO"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . isPermutation @Int)]

sameLength :: () -> PropertyM IO Property
sameLength _ = do
    let v :: Vector Int = V.fromList [1..1000000]
    v' <- run $ shuffleIO v
    pure $ length v === length v'


isPermutation :: forall a . (Ord a , Show a, Arbitrary a) => Vector a -> PropertyM IO Property
isPermutation v =
  do
    v'  <- run $ shuffleIO v
    let ls  = V.toList v
    let ls' = V.toList v'
    pure $ (sort ls) === (sort ls')

