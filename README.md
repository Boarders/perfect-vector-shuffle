## `perfect-vector-shuffle`

A library providing Fischer--Yates in-place shuffles and related functionality.

As an example of how to use it:

```haskell
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

```

```haskell
    > main
      [2,8,1,5,10,9,7,3,6,4]
      [6,8,4,10,9,2,5,7,3,1]
      [8,5,4,1,10,9,3,6,2,7]

    > main
      [7,9,3,5,10,6,8,1,2,4]
      [2,4,10,7,8,1,5,9,3,6]
      [4,8,5,2,7,3,9,6,10,1]
```
