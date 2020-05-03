module Main where

import qualified Immutable.Test as Immutable
import qualified Mutable.Test   as Mutable
import           Test.Tasty


main :: IO ()
main = defaultMain  testSuite


testSuite :: TestTree
testSuite = testGroup "Perfect Vector Sort"
  [ Immutable.testSuite
  , Mutable.testSuite
  ]
