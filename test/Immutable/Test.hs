{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Immutable.Test
  ( testSuite
  ) where

import           Data.List                        (sort)
import           Data.Vector                      (Vector, (!))
import qualified Data.Vector                      as V
import           Immutable.Shuffle
import           Test.QuickCheck.Instances.Vector ()
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck            as QC hiding (shuffle)


testSuite :: TestTree
testSuite = testGroup ""
  [ localOption (QuickCheckTests 10000) shuffleMTestSuite
  , localOption (QuickCheckTests 0    ) performanceTest
  , localOption (QuickCheckTests 10000) maximalCycleTestSuite
  , localOption (QuickCheckTests 10000) derangementTestSuite
  ]

performanceTest :: TestTree
performanceTest = testGroup "Performance"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . sameLength)]

shuffleMTestSuite :: TestTree
shuffleMTestSuite = testGroup "shuffleM"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . isPermutation @Int)]

maximalCycleTestSuite :: TestTree
maximalCycleTestSuite = testGroup "maximalCycleM"
  [ QC.testProperty
      "maximal cycle does indeed produce a maximal cycle on [0..n]"
      (monadicIO . isMaximalCycle)]


derangementTestSuite :: TestTree
derangementTestSuite = testGroup "derangementM"
  [ QC.testProperty
      "derangement does indeed produce a derangment on [0..n]."
      (monadicIO . isDerangement)]

sameLength :: () -> PropertyM IO Property
sameLength _ = do
    let v :: Vector Int = V.fromList [1..1000000]
    v' <- run $ shuffleM v
    pure $ length v === length v'


isPermutation :: forall a . (Ord a , Show a, Arbitrary a) => Vector a -> PropertyM IO Property
isPermutation v =
  do
    v'  <- run $ shuffleM v
    let ls  = V.toList v
    let ls' = V.toList v'
    pure $ sort ls === sort ls'


isMaximalCycle :: Positive Int -> PropertyM IO Property
isMaximalCycle (Positive n) =
  do
    v <- run $ maximalCycleM (V.fromList [0..n])
    pure $ cycleLength v === (n + 1)

   where
     cycleLength :: Vector Int -> Int
     cycleLength v = go (V.head v) v

     go :: Int -> Vector Int -> Int
     go k xs = if k == 0
                       then 1
                     else
                       1 + go (xs ! k) xs

isDerangement :: Positive Int -> PropertyM IO Property
isDerangement (Positive n) =
  do
    v <- run $ derangementM (V.fromList [0.. n])
    let perm    = V.indexed v
    let unmoved = V.filter (uncurry (==)) perm
    pure $ null unmoved === True

