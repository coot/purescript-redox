module Test.Main where

import Prelude
import Test.Free (testSuite) as Free
import Test.Store (testSuite) as Store
import Test.Unit.Main (runTest)
import Effect (Effect)

main :: Effect Unit
main = runTest do
  Free.testSuite
  Store.testSuite

