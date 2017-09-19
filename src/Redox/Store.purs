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
  , getSubscriptions
  , modifyStore
  , mkStore
  , mkStoreG
  , performRedoxEff
  , setState
  , subscribe
  , unsubscribe
  ) where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Data.Newtype (class Newtype, un)
import Data.Traversable (traverse_)
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
foreign import mkStoreImpl
  :: forall state e eff
   . EffFn1 (redox :: RedoxStore (create :: CreateRedox | e) | eff)
      state
      (Store state)

mkStore
  :: forall m state e eff
   . MonadEff (redox :: RedoxStore (create :: CreateRedox | e) | eff) m
  => state
  -> m (Store state)
mkStore s = liftEff $ runEffFn1 mkStoreImpl s

newtype SubscriptionId = SubscriptionId Int

derive instance newtypeSubsctiptionId :: Newtype SubscriptionId _

derive instance eqSubScriptionId :: Eq SubscriptionId

derive instance ordSubscriptionId :: Ord SubscriptionId

foreign import subscribeImpl
  :: forall state e eff eff'
   . EffFn2 (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff)
      (Store state) 
      (state -> Eff eff' Unit)
      Int

-- | Subscribe to store updates.  Note that store updates are not run by the
-- | store itself.  That is left to dispatch or the DSL interpreter.
-- | It returns id of the subscribed callback.  You can use it to remove the subscription.
subscribe
  :: forall m state e eff eff'
   . MonadEff (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) m
  => Store state
  -> (state -> Eff eff' Unit)
  -> m SubscriptionId
subscribe s fn = liftEff $ SubscriptionId <$> (runEffFn2 subscribeImpl s fn)

foreign import unsubscribeImpl
  :: forall state e eff
   . EffFn2 (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) (Store state) Int Unit

-- | Remove a subscription with a given id.
unsubscribe
  :: forall m state e eff
   . MonadEff (redox :: RedoxStore (subscribe :: SubscribeRedox | e) | eff) m
  => Store state
  -> SubscriptionId
  -> m Unit
unsubscribe s i = liftEff $ runEffFn2 unsubscribeImpl s (un SubscriptionId i)

foreign import modifyStoreImpl
  :: forall state state' e eff
   . EffFn2 (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff)
      (state -> state')
      (Store state)
      (Store state')

modifyStore
  :: forall m state state' e eff
   . MonadEff (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff) m
  => (state -> state')
  -> Store state
  -> m (Store state')
modifyStore fn s = liftEff $ runEffFn2 modifyStoreImpl fn s

foreign import setStateImpl
  :: forall state e eff
   . EffFn2 (redox :: RedoxStore (write :: WriteRedox | e) | eff)
      (Store state)
      state
      (Store state)

setState
  :: forall m state e eff
   . MonadEff (redox :: RedoxStore (write :: WriteRedox | e) | eff) m
  => Store state
  -> state
  -> m (Store state)
setState st s = liftEff $ runEffFn2 setStateImpl st s

foreign import getStateImpl
  :: forall state e eff
   . EffFn1 (redox :: RedoxStore (read :: ReadRedox | e) | eff) (Store state) state

getState
  :: forall m state e eff
   . MonadEff (redox :: RedoxStore (read :: ReadRedox | e) | eff) m
  => Store state
  -> m state
getState store  = liftEff (runEffFn1 getStateImpl store)

foreign import getSubscriptionsImpl
  :: forall state e eff
   . EffFn1(redox :: RedoxStore (read :: ReadRedox | e) | eff)
      (Store state)
      (Array (state -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit))

-- | Get subscriptions.
getSubscriptions
  :: forall m state e eff
   . MonadEff (redox :: RedoxStore (read :: ReadRedox | e) | eff) m
  => Store state
  -> m (Array (state -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit))
getSubscriptions s = liftEff $ runEffFn1 getSubscriptionsImpl s

performRedoxEff :: forall a e. Eff (redox :: RedoxStore e) a -> a
performRedoxEff = unsafeCoerce unsafePerformEff

-- | Make store outside of Eff monad (global).
mkStoreG :: forall state. state -> Store state
mkStoreG = performRedoxEff <<< mkStore'
  where
    mkStore' :: state -> Eff (redox :: RedoxStore (create :: CreateRedox)) (Store state)
    mkStore' = unsafeCoerceEff <<< mkStore
