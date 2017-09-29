module Test.Free where 

import Prelude

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (Var, get, makeVar, set)
import Control.Monad.Free (Free, liftF)
import Data.Time.Duration (Milliseconds(..))
import Redox (addLogger, subscribe)
import Redox.Free (dispatch, dispatchP)
import Redox.Store (CreateRedox, ReadRedox, RedoxStore, Store, SubscribeRedox, WriteRedox, getState, mkStore)
import Redox.Utils (mkIncInterp, runSubscriptions)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Unsafe.Coerce (unsafeCoerce)

foreign import unsafeLog :: forall a e. a -> Eff e Unit

-- | DSL commands
data Command a
  = Increment Int a
  | IncrementSync Int a

derive instance functorCommand :: Functor Command

-- | This is the type for the abstract syntax tree of our DSL.  `Free` is
-- | a tree type, and a monad.  The monadic property of it gives a nice way of
-- | writting programs simply by using `liftF` and writting abstract syntax tree
-- | directly.  That's nice, since you don't need to write tokenizer, parser
-- | and all that machinery to get an interpreted language within purescript.
type DSL a = Free Command a

-- | The following set of function lifts `Command` to the `Free` monad.  This
-- | is gives API from which you can build complex programs using your the DSL.
-- | See `cmds` and `cmds2` below
increment :: Int -> DSL (Int -> Int)
increment i = liftF (Increment i id)

incrementSync :: Int -> DSL (Int -> Int)
incrementSync i = liftF (IncrementSync i id)

-- | We need an interpreter.  We start with a dual functor: `Command` had
-- | a sum type, hence `Run` has to be a product.  We use `Aff` since we want
-- | to interpret the `DSL` in the `Aff` monad.
newtype Run eff a = Run
  { increment :: Int -> Aff eff a
  , incrementSync :: Int -> Aff eff a
  }

derive instance functorRun :: Functor (Run eff)

-- | Basic interpreter.  We only specify how the state will be updated.
-- | Cofree can be seen as an annotated tree, where each node holds the
-- | value of current computation (the state), and the branching is done via
-- | supplied functor, here `Run`.  `Cofree` is an infinite data structure
-- | hence it will be able to interpret program of any length (or depth)
-- | written using the `Free` monad.
type Interp eff a = Cofree (Run eff) a

-- | Use `unfoldCofree` to create the interpreter.
mkInterp :: forall rx eff. Store Int -> Int -> Interp (redox :: RedoxStore (read :: ReadRedox | rx) | eff) Int
mkInterp store state = unfoldCofree id next state
  where
    next :: Int -> Run (redox :: RedoxStore (read :: ReadRedox | rx) | eff) Int
    next st = Run { increment: _increment
                  , incrementSync: _incrementSync
                  }

    _increment b = do
      a <- getState store
      delay $ Milliseconds 0.0
      pure (a + b)

    _incrementSync b = do
      a <- getState store
      pure (a + b)

-- | We need to pair the `Command` and `Run`.  This makes `Run` a dual to
-- | `Command`.  And you can see why `Run` has to have a product type.
pair :: forall eff x y. Command (x -> y) -> Run eff x -> Aff eff y
pair (Increment a next) (Run interp) = next <$> (interp.increment a)
pair (IncrementSync a next) (Run interp) = next <$> (interp.incrementSync a)

-- | This function will be passed to `Redox.dispatch` to interpret `DSL`
-- | program. You can use `Redox.dispatch` to run programs with this
-- | interpreter. It will update the store when a DSL program finishes.
runInterp :: forall rx eff. Store Int -> DSL (Int -> Int) -> Int -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) Int
runInterp store cmds state =
  exploreM pair cmds $ mkInterp store state

-- | `runIncInterp` is an enhanced version of `runInterp` which updates the
-- | store on every step of computation. You can use `Redox.dispatchP` to run
-- | DSL programs.
runIncInterp :: forall rx eff. Store Int -> DSL (Int -> Int) -> Int -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) Int
runIncInterp store cmds state =
  exploreM pair cmds $ (addLogger unsafeCoerce <<< mkIncInterp store <<< mkInterp store) state

-- | `runSubscriptions` run all store subscriptions on each leaf of interpreter
runIncWithSubsInterp :: forall rx eff. Store Int -> DSL (Int -> Int) -> Int -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) Int
runIncWithSubsInterp store cmds state =
  exploreM pair cmds $ (runSubscriptions store <<< mkIncInterp store <<< mkInterp store) state

-- | `cmds` is a simple program in our DSL
cmds1 :: DSL (Int -> Int)
cmds1 = do 
  _ <- increment 10
  increment (-5)

-- | another DSL program
cmds2 :: DSL (Int -> Int)
cmds2 = do
  _ <- incrementSync 10
  incrementSync (-5)

cmds3 :: DSL (Int -> Int)
cmds3 = do
  _ <- increment 10
  pure (_ + 1)

-- counter
foreign import data COUNT :: Effect
foreign import getCounter :: forall eff. Eff (count :: COUNT | eff) Int
foreign import setCounter :: forall eff. Int -> Eff (count :: COUNT | eff) Unit

counter :: forall eff. Var (count :: COUNT | eff) Int
counter = makeVar getCounter setCounter

testSuite :: forall eff. TestSuite (redox :: RedoxStore (create :: CreateRedox, read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox), count :: COUNT | eff)
testSuite =
  suite "DSL" do

    test "update store asynchronously" $ do
      store <- liftEff $ mkStore 0
      _ <- liftEff $ dispatch
        (\_ -> pure unit)
        (runInterp store)
        store
        cmds1
      state <- getState store
      assert "store updated asynchronously" (state == 0)

    test "update store" $ do
      store <- mkStore 0
      _ <- liftEff $ dispatch
        (\_ -> pure unit)
        (runIncInterp store)
        store
        cmds1
      delay $ Milliseconds 20.0
      state <- getState store
      assert ("store failed to update: " <> show state) (state == 5)

    test "run sync commands" $ do
      store <- mkStore 0
      _ <- liftEff $ dispatch
        (\_ -> pure unit)
        (runIncInterp store)
        store
        cmds2
      state <- getState store
      assert ("store should update " <> show state) (state == 5)

    test "non identity" $ do
      store <- mkStore 0
      _ <- liftEff $ dispatch
        (\_ -> pure unit)
        (runIncInterp store)
        store
        cmds3
      state <- do
        delay $ Milliseconds 10.0
        getState store
      assert ("wrong value "<> show state) (state == 11)

    test "incremental interpreter" $ do
      store <- mkStore 0
      _ <- liftEff $ dispatchP
        (\_ -> pure unit)
        (runIncInterp store)
        store
        do
          _ <- incrementSync 1
          _ <- increment 1
          increment 1
      state1 <- getState store
      assert ("store should update " <> show state1 <> " expected 1") (state1 == 1)
      state2 <- do
        delay $ Milliseconds 0.0
        getState store
      assert ("store should increment " <> show state2 <> " expected 2") (state2 == 2)
      state3 <- do
        delay $ Milliseconds 0.0
        getState store
      assert ("store should increment " <> show state3 <> " expected 3") (state3 == 3)

    test "runSubscriptions" $ do
      liftEff $ set counter 0
      store <- mkStore 0
      _ <- subscribe store
        (\_ -> do
          c <- get counter
          set counter (c + 1)
          pure unit
        )
      _ <- liftEff $ dispatchP
        (\_ -> pure unit)
        (runIncWithSubsInterp store)
        store
        do
          _ <- incrementSync 1
          incrementSync 1
      state <- getState store
      c <- liftEff $ get counter
      assert ("counter: got: " <> show c <> " expected: 2") $ c == 2

