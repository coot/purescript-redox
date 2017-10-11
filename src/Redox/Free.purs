module Redox.Free
  ( _dispatch
  , dispatch
  , dispatchP
  ) where

import Prelude

import Control.Monad.Aff (Aff, Fiber, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free)
import Data.Either (Either, either)
import Data.Traversable (traverse_)
import Redox.Store (ReadRedox, WriteRedox, RedoxStore, Store, setState)
import Redox.Store as O

type Interpret dsl state eff = Free dsl (state -> state) -> state -> Aff eff state

_dispatch
  :: forall state dsl eff e
   . (Either Error state -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit)
  -> Interpret dsl state (redox :: RedoxStore (read :: ReadRedox | e) | eff)
  -> Store state
  -> Free dsl (state -> state)
  -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) (Fiber (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit)
_dispatch fn interp store cmds =
  do
    state <- O.getState store
    runAff fn (interp cmds state)

-- | Dispatch dsl commands that will be interpreted in Aff monad.
-- | You have to write your own DSL for the state changes and an interpreter for it.
-- | Check out
-- | [purescript-dsl-example](https://github.com/coot/purescript-dsl-example) or
-- | see the tests.  This is useful if you want to have a batch dispatch that
-- | dispatches all the commands at once when the interpreter finished running
-- | your DSL.
dispatch
  :: forall state dsl eff e
   . (Error -> Eff (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff) Unit)
  -> Interpret dsl state (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff)
  -> Store state
  -> Free dsl (state -> state)
  -> Eff
      (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff)
      (Fiber (redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox | e) | eff) Unit)
dispatch errFn interp store cmds = _dispatch (either errFn succFn) (\dsl -> interp dsl) store cmds
  where
    succFn state = do
      -- update store state
      _ <- setState store state
      -- run subscriptions
      subs <- O.getSubscriptions store
      traverse_ (_ $ state) subs

-- | Dispatch function which does not handle store updates.  That's useful if
-- | the interpreter is updating the store. You can use
-- | `Redox.Utils.mkIncInterp` and `Redox.Utils.runSubscriptions` to create
-- | such an interpreter.
dispatchP
  :: forall state dsl eff e
   . (Error -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit)
  -> Interpret dsl state (redox :: RedoxStore (read :: ReadRedox | e) | eff)
  -> Store state
  -> Free dsl (state -> state)
  -> Eff (redox :: RedoxStore (read :: ReadRedox | e) | eff) (Fiber (redox :: RedoxStore (read :: ReadRedox | e) | eff) Unit)
dispatchP errFn interp store cmds =
  _dispatch (either errFn (const $ pure unit)) (\dsl -> interp dsl) store cmds
