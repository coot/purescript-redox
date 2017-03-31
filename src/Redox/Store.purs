module Redox.Store where

import Prim
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, newRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Unsafe.Coerce (unsafeCoerce)

foreign import data REDOX :: !

foreign import data Store :: Type -> Type

-- | Make store with initial state.
foreign import mkStore :: forall state eff. state -> Eff (redox :: REDOX | eff) (Store state)

-- | Subscribe to store updates.  Not that store updates are not run by the
-- | store itself.  That is left to dispatch implementations.
foreign import subscribe :: forall state eff. Store state -> (state -> Eff (redox :: REDOX | eff) Unit) -> Eff (redox :: REDOX | eff) Unit

foreign import unsubscribe :: forall state eff. Store state -> (state -> Eff (redox :: REDOX | eff ) Unit) -> Eff (redox :: REDOX | eff) Unit

foreign import mapStore :: forall state state' eff. (state -> state') -> Store state -> Eff (redox :: REDOX | eff) (Store state')

foreign import setState :: forall state eff. Store state -> state -> Eff (redox :: REDOX | eff) (Store state)

foreign import getState :: forall state eff. Store state -> Eff (redox :: REDOX | eff) state

-- | Get subscriptions.
foreign import getSubs :: forall state eff. Store state -> Eff (redox :: REDOX | eff) (Array (state -> Eff (redox :: REDOX | eff) Unit))

instance functorStore :: Functor Store where
  map fn store = unsafePerformEff $ mapStore fn store

performRedoxEff :: forall a. Eff (redox :: REDOX) a -> a
performRedoxEff = unsafeCoerce unsafePerformEff

-- | Make store outside of Eff monad (global)
mkStoreG :: forall state. state -> Store state
mkStoreG = performRedoxEff <<< mkStore'
  where
    mkStore' :: state -> Eff (redox :: REDOX) (Store state)
    mkStore' = unsafeCoerceEff <<< mkStore
