module Redox.Store where

import Prim
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Unsafe.Coerce (unsafeCoerce)

-- | Effect for creating Redox Store
foreign import data CreateRedox :: !

-- | Effect for reading state of the store or retreaving store subscribers.
foreign import data ReadRedox :: !

-- | Effect for writing to the store
foreign import data WriteRedox :: !

-- | Effect for (un)subscribing to the store
foreign import data SubscribeRedox :: !

type ReadOnlyRedox eff = (readRedox :: ReadRedox | eff)

type WriteOnlyRedox eff = (writeRedox :: WriteRedox | eff)

type ReadWriteRedox eff = (readRedox :: ReadRedox, writeRedox :: WriteRedox | eff)

type ReadWriteSubscribeRedox eff = (readRedox :: ReadRedox, writeRedox :: WriteRedox, subscribeRedox :: SubscribeRedox | eff)

foreign import data Store :: Type -> Type

-- | Make store with initial state. Store is a mutable container with
-- | a subscription mechanism.
foreign import mkStore :: forall state eff. state -> Eff (createRedox :: CreateRedox | eff) (Store state)

-- | Subscribe to store updates.  Note that store updates are not run by the
-- | store itself.  That is left to dispatch or the DSL interpreter.
foreign import subscribe :: forall state eff. Store state -> (state -> Eff (subscribeRedox :: SubscribeRedox | eff) Unit) -> Eff (subscribeRedox :: SubscribeRedox | eff) Unit

foreign import unsubscribe :: forall state eff. Store state -> (state -> Eff (subscribeRedox :: SubscribeRedox | eff ) Unit) -> Eff (subscribeRedox :: SubscribeRedox | eff) Unit

foreign import mapStore :: forall state state' eff. (state -> state') -> Store state -> Eff (ReadWriteRedox eff)  (Store state')

foreign import setState :: forall state eff. Store state -> state -> Eff (WriteOnlyRedox eff) (Store state)

foreign import getState :: forall state eff. Store state -> Eff (ReadOnlyRedox eff) state

-- | Get subscriptions.
foreign import getSubs :: forall state eff. Store state -> Eff (readRedox :: ReadRedox | eff) (Array (state -> Eff (readRedox :: ReadRedox | eff) Unit))

instance functorStore :: Functor Store where
  map fn store = unsafePerformEff $ mapStore fn store


performCreateRedoxEff :: forall a. Eff (createRedox :: CreateRedox) a -> a
performCreateRedoxEff = unsafeCoerce unsafePerformEff

performWriteRedoxEff :: forall a. Eff (WriteOnlyRedox ()) a -> a
performWriteRedoxEff = unsafeCoerce unsafePerformEff

performReadRedoxEff :: forall a. Eff (ReadOnlyRedox ()) a -> a
performReadRedoxEff = unsafeCoerce unsafePerformEff

performReadWriteRedoxEff :: forall a. Eff (ReadWriteRedox ()) a -> a
performReadWriteRedoxEff = unsafeCoerce unsafePerformEff

-- | Make store outside of Eff monad (global).
mkStoreG :: forall state. state -> Store state
mkStoreG = performCreateRedoxEff <<< mkStore'
  where
    mkStore' :: state -> Eff (createRedox :: CreateRedox) (Store state)
    mkStore' = unsafeCoerceEff <<< mkStore
