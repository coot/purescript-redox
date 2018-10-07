module Redox.Free
  ( _dispatch
  , dispatch
  , dispatchP
  ) where

import Prelude


import Effect.Aff (Aff, Fiber, runAff)
import Effect.Exception (Error)
import Control.Monad.Free (Free)
import Data.Either (Either, either)
import Data.Traversable (traverse_)
import Redox.Store (Store, getState, getSubscriptions, setState)
import Effect (Effect)

type Interpret dsl state = Free dsl (state -> state) -> state -> Aff state

_dispatch
  :: forall state dsl
   . (Either Error state -> Effect Unit)
  -> Interpret dsl state
  -> Store state
  -> Free dsl (state -> state)
  -> Effect (Fiber Unit)
_dispatch fn interp store cmds =
  do
    state <- getState store
    runAff fn (interp cmds state)

-- | Dispatch dsl commands that will be interpreted in Aff monad.
-- | You have to write your own DSL for the state changes and an interpreter for it.
-- | Check out
-- | [purescript-dsl-example](https://github.com/coot/purescript-dsl-example) or
-- | see the tests.  This is useful if you want to have a batch dispatch that
-- | dispatches all the commands at once when the interpreter finished running
-- | your DSL.
dispatch
  :: forall state dsl
   . (Error -> Effect Unit)
  -> Interpret dsl state
  -> Store state
  -> Free dsl (state -> state)
  -> Effect (Fiber Unit)
dispatch errFn interp store cmds = _dispatch (either errFn succFn) (\dsl -> interp dsl) store cmds
  where
    succFn state = do
      -- update store state
      _ <- setState store state
      -- run subscriptions
      subs <- getSubscriptions store
      traverse_ (_ $ state) subs

-- | Dispatch function which does not handle store updates.  That's useful if
-- | the interpreter is updating the store. You can use
-- | `Redox.Utils.mkIncInterp` and `Redox.Utils.runSubscriptions` to create
-- | such an interpreter.
dispatchP
  :: forall state dsl
   . (Error -> Effect Unit)
  -> Interpret dsl state
  -> Store state
  -> Free dsl (state -> state)
  -> Effect (Fiber Unit)
dispatchP errFn interp store cmds =
  _dispatch (either errFn (const $ pure unit)) (\dsl -> interp dsl) store cmds
