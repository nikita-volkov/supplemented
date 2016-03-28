module Main where

import Rebase.Prelude hiding (takeWhile)
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Supplemented


main =
  defaultMain $
  testGroup "All tests"
  [
    testCase "Supplements unite" $
    let
      result :: [Int]
      result =
        execWriter $
        runSupplemented $
        do
          essence (tell (pure 1))
          supplement (tell (pure 2))
          essence (tell (pure 3))
          supplement (tell (pure 4))
          supplement (tell (pure 5))
          pure ()
          supplement (tell (pure 6))
      in assertEqual (show result) [1, 2, 3] result
  ]
