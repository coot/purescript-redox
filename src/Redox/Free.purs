module Redox.Free
  ( _dispatch
  , dispatch
  , dispatchP
  , Interp
  ) where

import Prelude
import Redox.Store as O
import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free)
import Data.Foldable (sequence_)
import Redox.Store (ReadRedox, RedoxState, Store)

type Interp dsl state eff = Free dsl (state -> state) -> state -> Aff eff state

_dispatch
  :: forall state dsl eff e
   . (Error -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) Unit)
  -> (state -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) Unit)
  -> Interp dsl state (state :: RedoxState (read :: ReadRedox | e) | eff)
  -> Store state
  -> Free dsl (state -> state)
  -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) (Canceler (state :: RedoxState (read :: ReadRedox | e) | eff))
_dispatch errFn succFn interp store cmds =
  do
    state <- O.getState store
    runAff errFn succFn (interp cmds state)

-- | Dispatch dsl commands that will be interpreted in Aff monad.
-- | You have to write your own DSL for the state changes and an interpreter for it.
-- | Check out
-- | [purescript-dsl-example](https://github.com/coot/purescript-dsl-example) or
-- | see the tests.  This is useful if you want to have a batch dispatch that
-- | dispatches all the commands at once when the interpreter finished running
-- | over the commands.
dispatch
  :: forall state dsl eff e
   . (Error -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) Unit)
  -> Interp dsl state (state :: RedoxState (read :: ReadRedox | e) | eff)
  -> Store state
  -> Free dsl (state -> state)
  -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) (Canceler (state :: RedoxState (read :: ReadRedox | e) | eff))
dispatch errFn interp store cmds = _dispatch errFn succFn (\dsl -> interp dsl) store cmds
  where
    succFn state = do
      -- update store state
      _ <- pure $ (const state) <$> store
      -- run subscriptions
      subs <- O.getSubs store
      sequence_ ((_ $ state) <$> subs)

-- | Dispatch function which does not handle store updates.  That's useful if
-- | the interpreter is updating the store. You can use
-- | `Redox.Utils.mkIncInterp` to create such interpreter.
dispatchP
  :: forall state dsl eff e
   . (Error -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) Unit)
  -> Interp dsl state (state :: RedoxState (read :: ReadRedox | e) | eff)
  -> Store state
  -> Free dsl (state -> state)
  -> Eff (state :: RedoxState (read :: ReadRedox | e) | eff) (Canceler (state :: RedoxState (read :: ReadRedox | e) | eff))
dispatchP errFn interp store cmds =
  _dispatch errFn (\_ -> pure unit) (\dsl -> interp dsl) store cmds
