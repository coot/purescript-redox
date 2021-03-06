module TodoMVC.Components.Header where

import Prelude
import React (ReactClass, createClass, createElement, spec)
import React.DOM as R
import React.DOM.Props as P
import React.Redox (dispatch)
import ReactHocs.Context (accessContext)
import TodoMVC.Action (addTodo)
import TodoMVC.Components.TodoTextInput (todoTextInput)

header :: ReactClass Unit
header = accessContext $ createClass $ ((spec unit renderFn) { displayName = "Header" })
  where
    handleSave this text =
      if text /= ""
        then void $ dispatch this $ addTodo text
        else pure unit

    renderFn this = do
      pure $ R.header [ P.className "header" ]
        [ R.h1' [ R.text "todos" ]
        , createElement todoTextInput
          { onSave: handleSave this
          , newTodo: true
          , editing: false
          , placeholder: "What needs to be done?"
          , text: ""
          }
          []
        ]
