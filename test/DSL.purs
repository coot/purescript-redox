module Test.DSL where 

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, hoistCofree, unfoldCofree, head, tail, (:<))
import Control.Monad.Aff (Aff, forkAff, later, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Free (Free, liftF)
import Redox.Utils (mkIncInterp)
import Redox.DSL (dispatch, dispatchP)
import Redox.Store (Store, getState, mkStore, setState, ReadWriteRedox, ReadRedox, WriteRedox, CreateRedox, SubscribeRedox)
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert (assert)

foreign import unsafeLog :: forall a e. a -> Eff e Unit

-- | DSL commands
data Command a
  = Increment Int a
  | IncrementSync Int a

instance functorCommand :: Functor Command where
  map f (Increment i a) = Increment i (f a)
  map f (IncrementSync i a) = IncrementSync i (f a)

-- | This is the type for the abstract syntax tree of our DSL.  `Free` is
-- | a tree type, and a monad.  The monadic property of it gives a nice way of
-- | writting programs simply by using `liftF` and writting abstract syntax tree
-- | directly.  That's nice, since you don't need to write tokenizer, parser
-- | and all that machinery to get an interpreted language within purescript.
type DSL a = Free Command a

-- | The following set of function lifts `Command` to the `Free` monad.  This
-- | is gives API from which you can build complex programs using your the DSL.
-- | See `cmds` and `cmds2` below
increment :: Int -> DSL Unit
increment i = liftF (Increment i unit)

incrementSync :: Int -> DSL Unit
incrementSync i = liftF (IncrementSync i unit)

-- | We need an interpreter.  We start with a dual functor: `Command` had
-- | a sum type, hence `Run` has to be a product.  We use `Aff` since we want
-- | to interpret the `DSL` in the `Aff` monad.
newtype Run eff a = Run
  { increment :: Int -> Aff eff a
  , incrementSync :: Int -> Aff eff a
  }

instance functorRun :: Functor (Run eff) where
  map f (Run { increment, incrementSync }) = 
    Run { increment: map f <<< increment
        , incrementSync: map f <<< incrementSync
        }

-- | Basic interpreter.  We only specify how the state will be updated.
-- | Cofree can be seen as an annotated tree, where each node holds the
-- | value of current computation (the state), and the branching is done via
-- | supplied functor, here `Run`.  `Cofree` is an infinite data structure
-- | hence it will be able to interpret program of any length (or depth)
-- | written using the `Free` monad.
type Interp eff a = Cofree (Run eff) a

-- | Use `unfoldCofree` to create the interpreter.
mkInterp :: forall eff. Int -> Interp eff Int
mkInterp state = unfoldCofree state id next
  where
    next :: Int -> Run eff Int
    next st = Run { increment: increment st
                  , incrementSync: incrementSync st
                  }

    increment :: Int -> Int -> Aff eff Int
    increment a b = later $ pure (a + b)

    incrementSync :: Int -> Int -> Aff eff Int
    incrementSync a b = pure (a + b)

-- | We need to pair the `Command` and `Run`.  This makes `Run` a dual to
-- | `Command`.  And you can see why `Run` has to have a product type.
pair :: forall eff x y. Command (x -> y) -> Run eff x -> Aff eff y
pair (Increment a next) (Run interp) = next <$> (interp.increment a)
pair (IncrementSync a next) (Run interp) = next <$> (interp.incrementSync a)

-- | This function will be passed to `Redox.dispatch` to interpret `DSL`
-- | program. You can use `Redox.dispatch` to run programs with this
-- | interpreter. It will update the store when a DSL program finishes.
runInterp :: forall eff. DSL (Int -> Int) -> Int -> Aff eff Int
runInterp cmds state = exploreM pair cmds $ mkInterp state

-- | `runIncInterp` is an enhanced version of `runInterp` which updates the
-- | store on every step of computation. You can use `Redox.dispatchP` to run
-- | DSL programs.
runIncInterp :: forall eff. Store Int -> DSL (Int -> Int) -> Int -> Aff eff Int
runIncInterp store cmds state = exploreM pair cmds $ mkIncInterp store (mkInterp state)

-- | `cmds` is a simple program in our DSL
cmds :: DSL (Int -> Int)
cmds = do 
  increment 10
  increment (-5)
  pure id

-- | another DSL program
cmds2 :: DSL (Int -> Int)
cmds2 = do
  incrementSync 10
  incrementSync (-5)
  pure id

testSuite :: forall eff. TestSuite (readRedox :: ReadRedox, writeRedox :: WriteRedox, createRedox :: CreateRedox, subscribeRedox :: SubscribeRedox | eff)
testSuite =

  suite "DSL" do

    test "update store asynchronously" $ do
      store <- liftEff $ mkStore 0
      liftEff $ dispatch
        (\_ -> pure unit)
        runInterp
        store
        cmds
      state <- liftEff $ getState store
      assert "store updated synchronously" (state == 0)

    test "update store" $ do
      store <- liftEff $ mkStore 0
      liftEff $ dispatch
        (\_ -> pure unit)
        runInterp
        store
        cmds
      state <- later' 10 $ liftEff $ getState store
      assert ("store failed to update: " <> show state) (state == 5)

    test "run sync commands" $ do
      store <- liftEff $ mkStore 0
      liftEff $ dispatch
        (\_ -> pure unit)
        runInterp
        store
        cmds2
      state <- liftEff $ getState store
      assert ("store should update " <> show state) (state == 5)

    test "incremental interpreter" $ do
      store <- liftEff $ mkStore 0
      liftEff $ dispatchP
        (\_ -> pure unit)
        (runIncInterp store)
        store
        do
          incrementSync 1
          increment 1
          increment 1
          pure id
      state <- liftEff $ getState store
      assert ("store should update " <> show state) (state == 1)
      state <- later $ liftEff $ getState store
      assert ("store should increment " <> show state <> " expected 2") (state == 2)
      state <- later $ liftEff $ getState store
      assert ("store should increment " <> show state <> " expected 3") (state == 3)

