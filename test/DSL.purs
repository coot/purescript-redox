module Test.DSL 
  ( testSuite
  ) where 

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree, (:<))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Free (Free, liftF)
import Redox.DSL (dispatch)
import Redox.Store (REDOX, getState, mkStore)
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert (assert)

data Command a
  = Increment Int a
  | Decrement Int a

instance functorCommand :: Functor Command where
  map f (Increment i a) = Increment i (f a)
  map f (Decrement i a) = Decrement i (f a)

type DSL a = Free Command a

increment :: Int -> DSL Unit
increment i = liftF (Increment i unit)

decrement :: Int -> DSL Unit
decrement i = liftF (Decrement i unit)


newtype Run eff a = Run
  { increment :: Int -> Aff eff a
  , decrement :: Int -> Aff eff a
  }

instance functorRun :: Functor (Run eff) where
  map f (Run { increment, decrement }) = 
    Run { increment: map f <<< increment
        , decrement: map f <<< decrement
        }

type Interp eff a = Cofree (Run eff) a

mkInterp :: forall eff. Int -> Interp eff Int
mkInterp state = unfoldCofree state id next
  where
    next :: Int -> Run eff Int
    next st = Run { increment: increment st, decrement: decrement st }

    increment :: Int -> Int -> Aff eff Int
    increment a b = pure (a + b)

    decrement :: Int -> Int -> Aff eff Int
    decrement a b = pure (a - b)

pair :: forall eff x y. Command (x -> y) -> Run eff x -> Aff eff y
pair (Increment a next) (Run interp) = next <$> (interp.increment a)
pair (Decrement a next) (Run interp) = next <$> (interp.decrement a)

runInterp :: forall eff. DSL (Int -> Int) -> Int -> Aff eff Int
runInterp cmds state = exploreM pair cmds $ mkInterp state

cmds :: DSL (Int -> Int)
cmds = do 
  increment 10
  decrement 5
  pure id

testSuite :: forall eff. TestSuite (redox :: REDOX | eff)
testSuite =
  suite "DSL" do
    test "increment store" $ do
      store <- liftEff $ mkStore 0
      liftEff $ dispatch
        (\_ _ -> pure unit)
        runInterp
        store
        cmds
      state <- liftEff $ getState store
      assert ("store failed to update: " <> show state) $ state == 5
