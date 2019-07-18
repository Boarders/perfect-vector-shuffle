module Main where

import           Data.Vector
import           Immutable.Shuffle

main :: IO ()
main =
  do
    shuffleMyVector >>= print
    cycleMyVector   >>= print
    derangeMyVector >>= print

myVector :: Vector Int
myVector = fromList [1..10]

shuffleMyVector :: IO (Vector Int)
shuffleMyVector = shuffleM myVector

cycleMyVector :: IO (Vector Int)
cycleMyVector = maximalCycleM myVector

derangeMyVector :: IO (Vector Int)
derangeMyVector = derangementM myVector
