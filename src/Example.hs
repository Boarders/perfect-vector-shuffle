module Example where

import           Data.Vector
import           Immutable.Shuffle

shuffleMyVector :: IO (Vector Int)
shuffleMyVector = shuffleM myVector

myVector :: Vector Int
myVector = fromList [1..10]
