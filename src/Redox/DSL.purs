module Redox.DSL where

import Prelude
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Data.Foldable (sequence_)
import Data.Functor (mapFlipped)

import Redox.Store

-- | Dispatch dsl commands that will be interpreted in Aff monad.
-- | You have to write your own DSL for the state changes and an interpreter for it.
-- | Check out [purescript-dsl-example](https://github.com/coot/purescript-dsl-example).
dispatch
  :: forall state dsl eff
   . (dsl -> Error -> Eff (redox :: REDOX | eff) Unit)
  -> (dsl -> state -> Aff (redox :: REDOX | eff) state)
  -> Store state
  -> dsl
  -> Eff (redox :: REDOX | eff) Unit
dispatch errFn interp store cmds =
  do
    state <- getState store
    void <$> runAff
      -- todo: error handling
      (errFn cmds)
      (\state -> do
        -- update store state
        pure $ (const state) <$> store
        -- run subscriptions
        subs <- getSubs store
        sequence_ ((_ $ state) <$> subs)
      )
      -- interpret dsl commands
      $ interp cmds state
