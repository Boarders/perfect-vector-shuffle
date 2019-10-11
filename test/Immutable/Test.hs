{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Immutable.Test
  ( testSuite
  ) where

import           Data.List                        (sort)
import           Data.Vector                      (Vector, (!))
import qualified Data.Vector                      as V
import           Immutable.Shuffle
import           System.Random
import           Test.QuickCheck.Instances.Vector ()
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck            as QC hiding (shuffle)


testSuite :: TestTree
testSuite = testGroup ""
  [ localOption (QuickCheckTests 10000) shuffleTestSuite
  , localOption (QuickCheckTests 0    ) performanceTest
  , localOption (QuickCheckTests 10000) maximalCycleTestSuite
  , localOption (QuickCheckTests 10000) derangementTestSuite
  ]

performanceTest :: TestTree
performanceTest = testGroup "Performance"
  [ QC.testProperty
      "Shuffling preserves length and elements"
      (monadicIO . sameLength)]

shuffleTestSuite :: TestTree
shuffleTestSuite = testGroup "shuffleM"
  [ QC.testProperty
      "shuffleM: Shuffling preserves length and elements"
       (monadicIO . isPermutationM @Int)
  , QC.testProperty
      "shuffle: Shuffling preserves length and elements"
       (monadicIO . isPermutation  @Int)
  ]

maximalCycleTestSuite :: TestTree
maximalCycleTestSuite = testGroup "maximalCycleM"
  [ QC.testProperty
      "maximalCycleM: maximal cycle does indeed produce a maximal cycle on [0..n]"
      (monadicIO . isMaximalCycleM)
  , QC.testProperty
      "maximalCycle: maximal cycle does indeed produce a maximal cycle on [0..n]"
      (monadicIO . isMaximalCycle)
  ]


derangementTestSuite :: TestTree
derangementTestSuite = testGroup "derangementM"
  [ QC.testProperty
      "derangementM: derangement does indeed produce a derangment on [0..n]."
      (monadicIO . isDerangementM)
  , QC.testProperty
      "derangement: derangement does indeed produce a derangment on [0..n]."
      (monadicIO . isDerangement)
  ]

sameLength :: () -> PropertyM IO Property
sameLength _ = do
    let v :: Vector Int = V.fromList [1..1000000]
    v' <- run $ shuffleM v
    pure $ length v === length v'


isPermutationM :: forall a . (Ord a , Show a, Arbitrary a) => Vector a -> PropertyM IO Property
isPermutationM v =
  do
    v'  <- run $ shuffleM v
    let ls  = V.toList v
    let ls' = V.toList v'
    pure $ sort ls === sort ls'


isPermutation :: forall a . (Ord a , Show a, Arbitrary a) => Vector a -> PropertyM IO Property
isPermutation v =
  do
    g        <- run getStdGen
    let (v', _) = shuffle v g
    let ls  = V.toList v
    let ls' = V.toList v'
    pure $ sort ls === sort ls'


isMaximalCycleM :: Positive Int -> PropertyM IO Property
isMaximalCycleM (Positive n) =
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

isMaximalCycle :: Positive Int -> PropertyM IO Property
isMaximalCycle (Positive n) =
  do
    g      <- run getStdGen
    let (v, _) = maximalCycle (V.fromList [0..n]) g
    pure $ cycleLength v === (n + 1)

   where
     cycleLength :: Vector Int -> Int
     cycleLength v = go (V.head v) v

     go :: Int -> Vector Int -> Int
     go k xs = if k == 0
                       then 1
                     else
                       1 + go (xs ! k) xs


isDerangementM :: Positive Int -> PropertyM IO Property
isDerangementM (Positive n) =
  do
    v <- run $ derangementM (V.fromList [0..n])
    let perm    = V.indexed v
    let unmoved = V.filter (uncurry (==)) perm
    pure $ null unmoved === True


isDerangement :: Positive Int -> PropertyM IO Property
isDerangement (Positive n) =
  do
    g      <- run getStdGen
    let (v, _)  = derangement (V.fromList [0.. n]) g
    let perm    = V.indexed v
    let unmoved = V.filter (uncurry (==)) perm
    pure $ null unmoved === True

