module Redox.DSL 
  ( dispatch
  , dispatchP
  ) where

import Prelude
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Data.Foldable (sequence_)
import Data.Functor (mapFlipped)

import Redox.Store

_dispatch
  :: forall state dsl eff
   . (Error -> Eff (redox :: REDOX | eff) Unit)
  -> (state -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) Unit
_dispatch errFn succFn interp store cmds =
  do
    state <- getState store
    void $ runAff errFn succFn (interp cmds state)

-- | Dispatch dsl commands that will be interpreted in Aff monad.
-- | You have to write your own DSL for the state changes and an interpreter for it.
-- | Check out [purescript-dsl-example](https://github.com/coot/purescript-dsl-example).
dispatch
  :: forall state dsl eff
   . (Error -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) Unit
dispatch errFn interp store cmds = _dispatch errFn succFn interp store cmds
  where
    succFn state = do
      -- update store state
      pure $ (const state) <$> store
      -- run subscriptions
      subs <- getSubs store
      sequence_ ((_ $ state) <$> subs)

-- | Dispatch function which does not handle store updates
-- | That's useful if the interpreter is updating the store 
dispatchP
  :: forall state dsl eff
   . (Error -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) Unit
dispatchP errFn interp store cmds =
  _dispatch errFn (\_ -> pure unit) interp store cmds
