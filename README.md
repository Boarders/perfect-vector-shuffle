## `perfect-vector-shuffle`

A tiny library for performing a random shuffle on a vector in the same vein as [`random-shuffle`](http://hackage.haskell.org/package/random-shuffle-0.0.4) does for lists.

As an example of how to use it:
```
module Example where
 
import Data.Vector
import Immutable.Shuffle

shuffleMyVector :: IO (Vector Int)
shuffleMyVector = shuffleM myVector

myVector :: Vector Int
myVector = fromList [1..10]
```

```
> shuffleMyVector
[1,10,4,7,2,3,5,9,8,6]

>shuffleMyVector
[7,4,2,10,9,8,6,5,1,3]
```
