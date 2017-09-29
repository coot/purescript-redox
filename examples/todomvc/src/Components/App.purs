module TodoMVC.Components.App where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import React (ReactClass, createClass, createElement, spec)
import React.DOM (div')
import React.Redox (withStore)
import Redox (ReadRedox, RedoxStore, WriteRedox, mkStore)
import Redox (dispatch) as Redox
import Redox.Store (CreateRedox)
import TodoMVC.Components.Header (header)
import TodoMVC.Components.MainSection (mainSection)
import TodoMVC.Store (initialState, mkInterp, runAction)

app
  :: forall rx eff
   . Eff
      ( console :: CONSOLE
      , redox :: RedoxStore ( create :: CreateRedox, read :: ReadRedox, write :: WriteRedox | rx )
      | eff )
      (ReactClass Unit)
app = do
  store <- mkStore initialState
  interp <- mkInterp store
  withStore store
    (Redox.dispatch (error <<< show) (runAction interp))
    (createClass $ ((spec unit renderFn) { displayName = "App" }))
  where
    renderFn this = do
      pure $ div'
        [ createElement header unit []
        , createElement mainSection unit []
        ]

