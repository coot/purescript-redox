module Redox where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, newRef)

foreign import data REDOX :: !

foreign import data Store :: * -> *

foreign import createStore :: forall state eff. state -> Eff (redox :: REDOX | eff) (Store state)

foreign import subscribe :: forall state eff. Store state -> (state -> Eff (redox :: REDOX | eff) Unit) -> Eff (redox :: REDOX | eff) Unit

foreign import unsubscribe :: forall state eff. Store state -> (state -> Eff (redox :: REDOX | eff ) Unit) -> Eff (redox :: REDOX | eff) Unit

foreign import mapStore :: forall state eff. Store state -> (state -> state) -> Eff (redox :: REDOX | eff) Unit
