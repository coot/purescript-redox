module Test.Store
  (testSuite)
  where

import Prelude
import Redox.Store
import Data.Array as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert (assert)

foreign import eqFns :: forall a eff. (a -> Eff eff Unit) -> (a -> Eff eff Unit) -> Boolean

testSuite :: forall eff. TestSuite (redox :: REDOX | eff)
testSuite = do

  suite "Store" $ do

    suite "mkStore" $ do

      test "mkStore inital state" $ do
        state <- liftEff $ mkStore 0 >>= getState
        assert ("wrong initial store value " <> show state) (state == 0)

      test "mkStore inital subscriptions" $ do
        subs <- liftEff $ mkStore 0 >>= getSubs
        assert ("non empty initial subscriptions " <> show (A.length subs)) (A.null subs)

    suite "Functor Store" $ do

      test "should map over Store" $ do
        state <- liftEff $ mkStore 0 >>= (pure <<< map (add 1)) >>= getState
        assert ("store did not update its state " <> show state) (state == 1)

    suite "subscriptions" $ do

      test "add subscription" $ 
        let fn = const (pure unit :: Eff (redox :: REDOX | eff) Unit)
        in do
          store <- liftEff $ mkStore 0
          liftEff $ subscribe store fn

          subs <- liftEff $ getSubs store
          assert ("store should have one subscription") $ (A.length subs == 1)
          case A.head subs of
            Nothing -> failure "ups..."
            Just fn' -> assert ("it should be the supplied fn") $ (eqFns fn fn')

