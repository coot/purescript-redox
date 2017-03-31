module Test.DSL 
  ( testSuite
  ) where 

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, hoistCofree, unfoldCofree, head, tail, (:<))
import Control.Monad.Aff (Aff, forkAff, later, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Free (Free, liftF)
import Debug.Trace (trace)
import Redox.DSL (dispatch, dispatchP)
import Redox.Store (REDOX, Store, getState, mkStore, setState)
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert (assert)

foreign import unsafeLog :: forall a e. a -> Eff e Unit

data Command a
  = Increment Int a
  | Decrement Int a
  | IncrementSync Int a
  | DecrementSync Int a

instance functorCommand :: Functor Command where
  map f (Increment i a) = Increment i (f a)
  map f (Decrement i a) = Decrement i (f a)
  map f (IncrementSync i a) = IncrementSync i (f a)
  map f (DecrementSync i a) = DecrementSync i (f a)

type DSL a = Free Command a

increment :: Int -> DSL Unit
increment i = liftF (Increment i unit)

decrement :: Int -> DSL Unit
decrement i = liftF (Decrement i unit)

incrementSync :: Int -> DSL Unit
incrementSync i = liftF (IncrementSync i unit)

decrementSync :: Int -> DSL Unit
decrementSync i = liftF (DecrementSync i unit)

newtype Run eff a = Run
  { increment :: Int -> Aff eff a
  , decrement :: Int -> Aff eff a
  , incrementSync :: Int -> Aff eff a
  , decrementSync :: Int -> Aff eff a
  }

instance functorRun :: Functor (Run eff) where
  map f (Run { increment, decrement, incrementSync, decrementSync }) = 
    Run { increment: map f <<< increment
        , decrement: map f <<< decrement
        , incrementSync: map f <<< incrementSync
        , decrementSync: map f <<< decrementSync
        }

type Interp eff a = Cofree (Run eff) a

mkInterp :: forall eff. Int -> Interp eff Int
mkInterp state = unfoldCofree state id next
  where
    next :: Int -> Run eff Int
    next st = Run { increment: increment st
                  , decrement: decrement st
                  , incrementSync: incrementSync st
                  , decrementSync: decrementSync st
                  }

    increment :: Int -> Int -> Aff eff Int
    increment a b = later $ pure (a + b)

    decrement :: Int -> Int -> Aff eff Int
    decrement a b = later $ pure (a - b)

    incrementSync :: Int -> Int -> Aff eff Int
    incrementSync a b = pure (a + b)

    decrementSync :: Int -> Int -> Aff eff Int
    decrementSync a b = pure (a - b)

pair :: forall eff x y. Command (x -> y) -> Run eff x -> Aff eff y
pair (Increment a next) (Run interp) = next <$> (interp.increment a)
pair (Decrement a next) (Run interp) = next <$> (interp.decrement a)
pair (IncrementSync a next) (Run interp) = next <$> (interp.incrementSync a)
pair (DecrementSync a next) (Run interp) = next <$> (interp.decrementSync a)

runInterp :: forall eff. DSL (Int -> Int) -> Int -> Aff eff Int
runInterp cmds state = exploreM pair cmds $ mkInterp state

-- unlike mkInter, this interpreter will update store in every step of computation
mkIncInterp :: forall eff. Store Int -> Int -> Interp (redox :: REDOX | eff) Int
mkIncInterp store state = hoist nat (mkInterp state)
  where
    -- like hoistCofree, but we don't have natural transformation here
    hoist nat cf = head cf :< nat (hoist nat <$> tail cf)

    nat
      :: forall state eff'
       . Run eff' (Interp eff' state)
      -> Run eff' (Interp eff' state)
    nat (Run r) = Run { increment: wrap r.increment
                      , decrement: wrap r.decrement
                      , incrementSync: wrap r.incrementSync
                      , decrementSync: wrap r.decrementSync
                      }
      where
        wrap fn = \arg ->
                  do
                    interp <- fn arg
                    let state = head interp
                    pure $ (const state) <$> store
                    pure interp

runIncInterp :: forall eff. Store Int -> DSL (Int -> Int) -> Int -> Aff (redox :: REDOX | eff) Int
runIncInterp store cmds state = exploreM pair cmds $ mkIncInterp store state

cmds :: DSL (Int -> Int)
cmds = do 
  increment 10
  decrement 5
  pure id

cmds2 :: DSL (Int -> Int)
cmds2 = do
  incrementSync 10
  decrementSync 5
  pure id

testSuite :: forall eff. TestSuite (redox :: REDOX | eff)
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

    -- note: if one mixes sync and async commands, they will all run when the
    -- last async command resolves - due to how Aff bind works.

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

