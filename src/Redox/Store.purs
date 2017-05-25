module Redox.Store
  ( RedoxStore
  , CreateRedox
  , ReadRedox
  , WriteRedox
  , SubscribeRedox
  , ReadOnlyRedox
  , WriteOnlyRedox
  , ReadWriteRedox
  , ReadWriteSubscribeRedox
  , CreateReadWriteSubscribeRedox
  , REDOX
  , Store
  , SubscriptionId(..)
  , getState
  , getSubs
  , mapStore
  , mkStore
  , mkStoreG
  , performRedoxEff
  , setState
  , subscribe
  , unsubscribe
  ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Newtype (class Newtype, wrap, unwrap)
import Unsafe.Coerce (unsafeCoerce)

-- | Effect for creating Redox Store
foreign import data CreateRedox :: Effect

-- | Effect for reading state of the store or retreaving store subscribers.
foreign import data ReadRedox :: Effect

-- | Effect for writing to the store
foreign import data WriteRedox :: Effect

-- | Effect for (un)subscribing to the store
foreign import data SubscribeRedox :: Effect

foreign import data RedoxStore :: # Effect -> Effect

type ReadOnlyRedox = (read :: ReadRedox)

type WriteOnlyRedox = (write :: WriteRedox)

type ReadWriteRedox = (read :: ReadRedox, write :: WriteRedox)

type ReadWriteSubscribeRedox = (read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox)

type CreateReadWriteSubscribeRedox = (create :: CreateRedox, read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox)

type REDOX = RedoxStore CreateReadWriteSubscribeRedox

foreign import data Store :: Type -> Type

-- | Make store with initial state. Store is a mutable container with
-- | a subscription mechanism.
foreign import mkStore :: forall state e eff. state -> Eff (redox :: RedoxStore (create :: CreateRedox | e) | eff) (Store state)

newtype SubscriptionId = SubscriptionId Int

derive instance newtypeSubsctiptionId :: Newtype SubscriptionId _

derive instance genericSubscriptionId :: Generic SubscriptionId

instance eqSubscriptionId :: Eq SubscriptionId where
  eq = gEq

instance ordSubscriptionId :: Ord SubscriptionId where
  compare = gCompare

foreign import _subscribe
  :: forall state e eff eff'
   . Store state
  -> (state -> Eff eff' Unit)
  -> Eff (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) Int

-- | Subscribe to store updates.  Note that store updates are not run by the
-- | store itself.  That is left to dispatch or the DSL interpreter.
-- | It returns id of the subscribed callback.  You can use it to remove the subscription.
subscribe
  :: forall state e eff eff'
   . Store state
  -> (state -> Eff eff' Unit)
  -> Eff (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) SubscriptionId
subscribe store fn = wrap <$> _subscribe store fn

foreign import _unsubscribe :: forall state e eff. Store state -> Int -> Eff (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) Unit

-- | Remove a subscription with a given id.
unsubscribe :: forall state e eff. Store state -> SubscriptionId -> Eff (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) Unit
unsubscribe store sid = _unsubscribe store $ unwrap sid

foreign import mapStore :: forall state state' e eff. (state -> state') -> Store state -> Eff (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff) (Store state')

foreign import setState :: forall state e eff. Store state -> state -> Eff (redox :: RedoxStore (write :: WriteRedox | e) | eff) (Store state)

foreign import getState :: forall state e eff. Store state -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) state

-- | Get subscriptions.
foreign import getSubs :: forall state e eff. Store state -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) (Array (state -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit))

instance functorStore :: Functor Store where
  map fn store = unsafePerformEff $ mapStore fn store

performRedoxEff :: forall a e. Eff (redox :: RedoxStore e) a -> a
performRedoxEff = unsafeCoerce unsafePerformEff

-- | Make store outside of Eff monad (global).
mkStoreG :: forall state. state -> Store state
mkStoreG = performRedoxEff <<< mkStore'
  where
    mkStore' :: state -> Eff (redox :: RedoxStore (create :: CreateRedox)) (Store state)
    mkStore' = unsafeCoerceEff <<< mkStore
