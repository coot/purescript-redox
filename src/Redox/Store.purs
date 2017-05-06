module Redox.Store
  ( REDOX
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

foreign import data REDOX :: Effect

foreign import data Store :: Type -> Type

-- | Make store with initial state. Store is a mutable container with
-- | a subscription mechanism.
foreign import mkStore :: forall state eff. state -> Eff (redox :: REDOX | eff) (Store state)

newtype SubscriptionId = SubscriptionId Int

derive instance newtypeSubsctiptionId :: Newtype SubscriptionId _

derive instance genericSubscriptionId :: Generic SubscriptionId

instance eqSubscriptionId :: Eq SubscriptionId where
  eq = gEq

instance ordSubscriptionId :: Ord SubscriptionId where
  compare = gCompare

foreign import _subscribe
  :: forall state eff eff'
   . Store state
  -> (state -> Eff eff' Unit)
  -> Eff (redox :: REDOX | eff) Int

-- | Subscribe to store updates.  Note that store updates are not run by the
-- | store itself.  That is left to dispatch or the DSL interpreter.
-- | It returns id of the subscribed callback.  You can use it to remove the subscription.
subscribe
  :: forall state eff eff'
   . Store state
  -> (state -> Eff eff' Unit)
  -> Eff (redox :: REDOX | eff) SubscriptionId
subscribe store fn = wrap <$> _subscribe store fn

foreign import _unsubscribe :: forall state eff. Store state -> Int -> Eff (redox :: REDOX | eff) Unit

-- | Remove a subscription with a given id.
unsubscribe :: forall state eff. Store state -> SubscriptionId -> Eff (redox :: REDOX | eff) Unit
unsubscribe store sid = _unsubscribe store $ unwrap sid

foreign import mapStore :: forall state state' eff. (state -> state') -> Store state -> Eff (redox :: REDOX | eff)  (Store state')

foreign import setState :: forall state eff. Store state -> state -> Eff (redox :: REDOX | eff) (Store state)

foreign import getState :: forall state eff. Store state -> Eff (redox :: REDOX | eff) state

-- | Get subscriptions.
foreign import getSubs :: forall state eff. Store state -> Eff (redox :: REDOX | eff) (Array (state -> Eff (redox :: REDOX | eff) Unit))

instance functorStore :: Functor Store where
  map fn store = unsafePerformEff $ mapStore fn store

performRedoxEff :: forall a. Eff (redox :: REDOX) a -> a
performRedoxEff = unsafeCoerce unsafePerformEff

-- | Make store outside of Eff monad (global).
mkStoreG :: forall state. state -> Store state
mkStoreG = performRedoxEff <<< mkStore'
  where
    mkStore' :: state -> Eff (redox :: REDOX) (Store state)
    mkStore' = unsafeCoerceEff <<< mkStore
