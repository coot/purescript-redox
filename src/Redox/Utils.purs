module Redox.Utils 
  ( addLogger
  , hoistCofree'
  , mkIncInterp
  , runSubscriptions
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.DateTime.Instant (Instant)
import Data.Traversable (sequence)
import Redox.Store (Store)
import Redox.Store as O

-- | A version of `hoistCofree`, where `nat` does not need to come from natural
-- | transformation.
-- | This corresponds to
-- | [applyMiddleware](https://github.com/reactjs/redux/blob/master/src/applyMiddleware.js) 
-- | in the [redux](https://github.com/reactjs/redux) library.
-- | You can use this function to add effects to your interpreter, like
-- | logging, optimistic updates, undo/redo stack, delayed actions... 
hoistCofree'
  :: forall f state
   . (Functor f)
  => (f (Cofree f state) -> f (Cofree f state))
  -> Cofree f state
  -> Cofree f state
hoistCofree' nat cf = head cf :< nat (hoistCofree' nat <$> tail cf)

-- | Make interpreter which updates the store on every step of computation.
-- | You have to supply the store and interperter of type `Cofree f state`.
-- | Check out tests how you can use it.  Note that it does not run
-- | subscriptions.  Use `runSubscriptions` for that.
mkIncInterp
  :: forall state f
   . (Functor f)
  => Store state
  -> Cofree f state
  -> Cofree f state
mkIncInterp store interp = hoistCofree' nat interp
  where
    nat :: f (Cofree f state) -> f (Cofree f state)
    nat fa = g <$> fa

    g :: Cofree f state -> Cofree f state
    g cof = O.performRedoxEff do
      cof <$ O.setState store (head cof)

-- | Run subscriptions on each leaf of the `Cofree` interpreter.  You'll likely
-- | want to use `mkIncInterp` first so that the subscriptions run on the updated
-- | state.
runSubscriptions
  :: forall state f
   . (Functor f)
  => Store state
  -> Cofree f state
  -> Cofree f state
runSubscriptions store interp = hoistCofree' nat interp
  where
    nat :: f (Cofree f state) -> f (Cofree f state)
    nat fa = g <$> fa

    g :: Cofree f state -> Cofree f state
    g cof = O.performRedoxEff do
      st <- O.getState store
      subs <- O.getSubscriptions store
      _ <- sequence ((_ $ st) <$> subs)
      pure cof

foreign import logValues :: forall a e. Array a -> Eff (console :: CONSOLE | e) Unit

foreign import formatInstant :: Instant -> String

-- | Add logger to the interpreter which logs updates on each
-- | leaf. 
-- |
-- | Note that leaves of the cofree interpreter might be visited more often
-- | than when subsciptions run.  If you are using `dispatch` without
-- | `mkIncInterp` or `runSubscriptions` the store will be updated only when
-- | leaves of `Free` DSL are reached, while this logger will log on
-- | every leaf of your cofree interpreter.
addLogger
  :: forall state f
   . Functor f
  => (state -> String)
  -> Cofree f state
  -> Cofree f state
addLogger toString = hoistCofree' (map nat)
  where
    nat :: Cofree f state -> Cofree f state
    nat cof = 
      let state = head cof
      in performEff do
        n <- now 
        logValues ["redox", formatInstant n, toString state]
        pure cof

    performEff :: forall a. Eff (console :: CONSOLE, now :: NOW) a -> a
    performEff = unsafePerformEff
