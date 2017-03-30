module Redox.Internals where

import Prim
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, newRef)

foreign import data REDOX :: !

foreign import data Store :: Type -> Type

foreign import createStore :: forall state eff. state -> Eff (redox :: REDOX | eff) (Store state)

foreign import subscribe :: forall state eff. Store state -> (state -> Eff (redox :: REDOX | eff) Unit) -> Eff (redox :: REDOX | eff) Unit

foreign import unsubscribe :: forall state eff. Store state -> (state -> Eff (redox :: REDOX | eff ) Unit) -> Eff (redox :: REDOX | eff) Unit

foreign import mapStore :: forall state state'. (state -> state') -> Store state -> Store state'

foreign import getState :: forall state eff. Store state -> Eff (redox :: REDOX | eff) state

foreign import getSubs :: forall state eff. Store state -> Eff (redox :: REDOX | eff) (Array (state -> Eff (redox :: REDOX | eff) Unit))

instance functorStore :: Functor Store where
  map fn store = mapStore fn store
