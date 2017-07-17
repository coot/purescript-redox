module Redox.Utils 
  ( addLogger
  , hoistCofree'
  , mkIncInterp
  , runSubscriptions
  ) where

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.DateTime.Instant (Instant)
import Data.Functor.Product (Product, product)
import Data.StrMap as SM
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Prelude (class Functor, Unit, bind, discard, flip, map, pure, ($), (*>), (<$), (<$>))
import Redox.Store (Store)
import Redox.Store as O
import Type.Row (class ListToRow, class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

-- | A version of `hoistCofree`, where `nat` does not need to come from
-- | a natural transformation.  This corresponds to
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

class LogActionRecord (i :: # Type) (o :: # Type)

instance logActionRecord
  :: ( RowToList i li
     , RowToList o lo
     , LogActionList li lo
     , ListToRow li i
     , ListToRow lo i
     )
  => LogActionRecord i o

class LogActionList (li :: RowList) (lo :: RowList)

instance logActionListNil :: LogActionList Nil Nil

instance logActionListCons
  :: LogActionList ti to
  => LogActionList (Cons k (Aff e a) ti) (Cons k (Aff e a) to)

logAction
  :: forall r r'
   . LogActionRecord r r'
  => Record r
  -> Record r'
logAction r = unsafeCoerce s'
  where
    s :: forall e1. SM.StrMap (Aff e1 Unit)
    s = unsafeCoerce r

    s' :: forall e2. SM.StrMap (Aff (console :: CONSOLE | e2) Unit)
    s' = SM.mapWithKey (\k m -> log k *> m) s


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
