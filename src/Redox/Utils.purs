module Redox.Utils 
  ( hoistCofree'
  , mkIncInterp
  , mkIncInterpNat
  , runSubscriptions
  , runSubscriptionsNat
  , addLogger
  , addLoggerNat
  , compose
  ) where

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Functor.Product (Product, product)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Prelude (class Functor, Unit, bind, discard, flip, map, pure, ($), (<$), (<$>))
import Redox.Store (Store)
import Redox.Store as O

-- | A version of `hoistCofree`, where `nat` does not need to come from
-- | a natural transformation.  This corresponds to
-- | [applyMiddleware](https://github.com/reactjs/redux/blob/master/src/applyMiddleware.js) 
-- | in the [redux](https://github.com/reactjs/redux) library.
-- | You can use this function to add effects to your interpreter, like
-- | logging, optimistic updates, undo/redo stack, delayed actions... 
hoistCofree'
  :: forall f state
   . Functor f
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
   . Functor f
  => Store state
  -> Cofree f state
  -> Cofree f state
mkIncInterp store = hoistCofree' (map $ mkIncInterpNat store)

-- | The `map $ mkIncInterpNat store` is the natural transformation that runs
-- | `mkIncInterp`.  If you have several middlewares to run using `hoistCofree`
-- | you can compose them first and run them once.
mkIncInterpNat
  :: forall state f
   . Functor f
  => Store state
  -> Cofree f state
  -> Cofree f state
mkIncInterpNat store cof =
  O.performRedoxEff $ cof <$ O.setState store (head cof)

-- | Run subscriptions on each leaf of the `Cofree` interpreter.  You'll likely
-- | want to use `mkIncInterp` first so that the subscriptions run on the updated
-- | state, e.g.
-- | ``` purescript
-- | runSubscriptions store <<< mkIncInterp store
-- | ```
-- | or even better
-- | ```
-- | hoistCofree' (map $ runSubscriptionsNat store <<< mkIncInterpNat store)
-- | ```
runSubscriptions
  :: forall state f
   . Functor f
  => Store state
  -> Cofree f state
  -> Cofree f state
runSubscriptions store = hoistCofree' (map $ runSubscriptionsNat store)

-- | The `map $ runSubscriptionsNat store` is the natural transformation of `f`
-- | that runs `runSubscriptions`.
runSubscriptionsNat
  :: forall f state
   . Functor f
  => Store state
  -> Cofree f state
  -> Cofree f state
runSubscriptionsNat store cof = O.performRedoxEff do
  st <- O.getState store
  subs <- O.getSubscriptions store
  _ <- sequence ((_ $ st) <$> subs)
  pure cof

foreign import logValues :: forall a e. Array a -> Eff (console :: CONSOLE | e) Unit

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
addLogger toString = hoistCofree' (map $ addLoggerNat toString)

performConsole :: forall a. Eff (console :: CONSOLE) a -> a
performConsole = unsafePerformEff

addLoggerNat
  :: forall f state
   . Functor f
  => (state -> String)
  -> Cofree f state
  -> Cofree f state
addLoggerNat toString cof = 
  let state = head cof
  in performConsole do
    logValues ["redox", toString state]
    pure cof

-- | Compose two interpreters. Check out an
-- | [example](http://try.purescript.org/?gist=b31f48d16ad43cec8c0afcd470ac5add)
compose
  :: forall f g a b
   . Functor f
  => Functor g
  => Cofree f a
  -> Cofree g b
  -> Cofree (Product f g) (Tuple a b)
compose f g =
  mkCofree
    (Tuple (head f) (head g))
    (fn (tail f) (tail g))
  where
    fn :: f (Cofree f a) -> g (Cofree g b) -> Product f g (Cofree (Product f g) (Tuple a b))
    fn fa gb = uncurry compose <$> (product (flip Tuple g <$> fa) (Tuple f <$> gb))
