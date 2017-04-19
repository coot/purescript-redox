module TodoMVC.Components.App where

import Prelude
import React (ReactClass, createClass, createElement, spec)
import React.DOM (div')
import React.Redox (withStore)
import Redox (dispatch) as Redox
import TodoMVC.Components.Header (header)
import TodoMVC.Components.MainSection (mainSection)
import TodoMVC.Store (runAction, store)

app :: ReactClass Unit
app = withStore
    store
    (Redox.dispatch (const $ pure unit) runAction)
    (createClass $ ((spec unit renderFn) { displayName = "App" }))
  where
    renderFn this = do
      pure $ div'
        [ createElement header unit []
        , createElement mainSection unit []
        ]

