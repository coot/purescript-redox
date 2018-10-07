module Redox.Store
  (
   Store
  , SubscriptionId(..)
  , getState
  , getSubscriptions
  , runStoreSubscriptions
  , modifyStore
  , mkStore
  , mkStoreG
  , setState
  , subscribe
  , unsubscribe
  ) where

import Prelude
import Data.Newtype (class Newtype, un)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Effect.Class (class MonadEffect, liftEffect)

foreign import data Store :: Type -> Type

-- | Make store with initial state. Store is a mutable container with
-- | a subscription mechanism.
foreign import mkStoreImpl
  :: forall state
   . EffectFn1
      state
      (Store state)

mkStore :: forall m state . MonadEffect m => state -> m (Store state)
mkStore s = liftEffect $ runEffectFn1 mkStoreImpl s

newtype SubscriptionId = SubscriptionId Int

derive instance newtypeSubsctiptionId :: Newtype SubscriptionId _

derive instance eqSubScriptionId :: Eq SubscriptionId

derive instance ordSubscriptionId :: Ord SubscriptionId

foreign import subscribeImpl :: forall state. EffectFn2 (Store state) (state -> Effect Unit) Int

-- | Subscribe to store updates.  Note that store updates are not run by the
-- | store itself.  That is left to dispatch or the DSL interpreter.
-- | It returns id of the subscribed callback.  You can use it to remove the subscription.
subscribe
  :: forall m state
   . MonadEffect m
  => Store state
  -> (state -> Effect Unit)
  -> m SubscriptionId
subscribe s fn = liftEffect $ SubscriptionId <$> (runEffectFn2 subscribeImpl s fn)

foreign import unsubscribeImpl
  :: forall state
   . EffectFn2 (Store state) Int Unit

-- | Remove a subscription with a given id.
unsubscribe
  :: forall m state
   . MonadEffect m
  => Store state
  -> SubscriptionId
  -> m Unit
unsubscribe s i = liftEffect $ runEffectFn2 unsubscribeImpl s (un SubscriptionId i)

foreign import modifyStoreImpl
  :: forall state state'
   . EffectFn2
      (state -> state')
      (Store state)
      (Store state')

modifyStore
  :: forall m state state'
   . MonadEffect m
  => (state -> state')
  -> Store state
  -> m (Store state')
modifyStore fn s = liftEffect $ runEffectFn2 modifyStoreImpl fn s

foreign import setStateImpl
  :: forall state
   . EffectFn2
      (Store state)
      state
      (Store state)

setState
  :: forall m state
   . MonadEffect m
  => Store state
  -> state
  -> m (Store state)
setState st s = liftEffect $ runEffectFn2 setStateImpl st s

foreign import getStateImpl
  :: forall state
   . EffectFn1 (Store state) state

getState
  :: forall m state
   . MonadEffect m
  => Store state
  -> m state
getState store  = liftEffect (runEffectFn1 getStateImpl store)

foreign import getSubscriptionsImpl :: forall state. EffectFn1(Store state) (Array (state -> Effect Unit))

-- | Get subscriptions.
getSubscriptions
  :: forall m state
   . MonadEffect m
  => Store state
  -> m (Array (state -> Effect Unit))
getSubscriptions s = liftEffect $ runEffectFn1 getSubscriptionsImpl s

runStoreSubscriptions
  :: forall m state
   . MonadEffect m
  => Store state
  -> m Unit
runStoreSubscriptions s = do
  subs <- getSubscriptions s
  state <- getState s
  liftEffect $ traverse_ (_ $ state) subs

-- | Make store outside of Eff monad (global).
mkStoreG :: forall state. state -> Store state
mkStoreG = unsafePerformEffect <<< mkStore'
  where
    mkStore' :: state -> Effect (Store state)
    mkStore' = mkStore
