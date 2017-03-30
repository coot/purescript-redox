module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.DSL (testSuite) as DSL
import Redox.Store (REDOX)

main :: forall eff. Eff (redox :: REDOX, avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT, err :: EXCEPTION | eff) Unit
main = runTest do
  DSL.testSuite
