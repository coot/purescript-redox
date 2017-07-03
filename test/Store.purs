module Test.Store
  (testSuite)
  where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Prelude (Unit, add, bind, const, discard, pure, show, unit, ($), (<>), (==), (>>=))
import Redox (WriteRedox, modifyStore)
import Redox.Store (CreateRedox, ReadRedox, RedoxStore, SubscribeRedox, getState, getSubscriptions, mkStore, subscribe, unsubscribe)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert (assert)

foreign import eqFns :: forall a eff. (a -> Eff eff Unit) -> (a -> Eff eff Unit) -> Boolean

testSuite :: forall e eff. TestSuite (redox :: RedoxStore (create :: CreateRedox, read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox | e) | eff)
testSuite = do

  suite "Store" do

    suite "mkStore" do

      test "mkStore inital state" do
        state <- liftEff $ mkStore 0 >>= getState
        assert ("wrong initial store value " <> show state) (state == 0)

      test "mkStore inital subscriptions" $ do
        subs <- liftEff $ mkStore 0 >>= getSubscriptions
        assert ("non empty initial subscriptions " <> show (A.length subs)) (A.null subs)

    suite "Functor Store" do

      test "should map over Store" $ do
        state <- liftEff $ mkStore 0 >>= (modifyStore (add 1)) >>= getState
        assert ("store did not update its state " <> show state) (state == 1)

    suite "subscriptions" do

      test "add subscription"
        let fn = const $ pure unit
        in do
          store <- liftEff $ mkStore 0
          _ <- liftEff $ subscribe store fn

          subs <- liftEff $ getSubscriptions store
          assert ("store should have one subscription") (A.length subs == 1)
          case A.head subs of
            Nothing -> failure "ups..."
            Just fn' -> assert ("it should be the supplied fn") $ eqFns fn fn'

      test "remove subscription"
        let fn = const $ pure unit
        in do
          store <- liftEff $ mkStore 0
          sId <- liftEff $ subscribe store fn
          liftEff $ unsubscribe store sId
          subs <- liftEff $ getSubscriptions store
          assert ("store should not have any subscriptions") (A.length subs == 0)

      test "remove multiple subscriptions"
        let fn = const (pure unit)
        in do
          store <- liftEff $ mkStore 0
          sId1 <- liftEff $ subscribe store fn
          sId2 <- liftEff $ subscribe store fn
          _ <- liftEff do
            unsubscribe store sId1
            unsubscribe store sId2
          subs <- liftEff $ getSubscriptions store
          assert ("store shoud not have any subscriptions, but has " <> show (A.length subs))  (A.length subs == 0)
            

