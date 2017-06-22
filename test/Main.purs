module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Redox.Store (ReadRedox, RedoxStore, WriteRedox, CreateRedox, SubscribeRedox)
import Test.Free (COUNT, testSuite) as Free
import Test.Store (testSuite) as Store
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. Eff (avar :: AVAR, console :: CONSOLE, err :: EXCEPTION, redox :: RedoxStore (create :: CreateRedox, read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox), testOutput :: TESTOUTPUT, count :: Free.COUNT | eff) Unit
main = runTest do
  Free.testSuite
  Store.testSuite
