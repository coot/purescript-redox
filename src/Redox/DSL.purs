module Redox.DSL 
  ( dispatch
  , dispatchP
  ) where

import Prelude
import Redox.Store
import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Data.Foldable (sequence_)

_dispatch
  :: forall state dsl eff
   . (Error -> Eff (redox :: REDOX | eff) Unit)
  -> (state -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) (Canceler (redox :: REDOX | eff))
_dispatch errFn succFn interp store cmds =
  do
    state <- getState store
    runAff errFn succFn (interp cmds state)

-- | Dispatch dsl commands that will be interpreted in Aff monad.
-- | You have to write your own DSL for the state changes and an interpreter for it.
-- | Check out [purescript-dsl-example](https://github.com/coot/purescript-dsl-example).
dispatch
  :: forall state dsl eff
   . (Error -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) (Canceler (redox :: REDOX | eff))
dispatch errFn interp store cmds = _dispatch errFn succFn interp store cmds
  where
    succFn state = do
      -- update store state
      pure $ (const state) <$> store
      -- run subscriptions
      subs <- getSubs store
      sequence_ ((_ $ state) <$> subs)

-- | Dispatch function which does not handle store updates.  That's useful if
-- | the interpreter is updating the store. You can use
-- | `Redox.Utils.mkIncInterp` to create such interpreter.
dispatchP
  :: forall state dsl eff
   . (Error -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) (Canceler (redox :: REDOX | eff))
dispatchP errFn interp store cmds =
  _dispatch errFn (\_ -> pure unit) interp store cmds
