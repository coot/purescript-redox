module TodoMVC.Components.TodoItem where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactHocs (accessContext)
import React.Redox (dispatch)

import Data.Array (singleton)
import TodoMVC.Types (Todo)
import TodoMVC.Action (addTodo, deleteTodo, editTodo, completeTodo)
import TodoMVC.Utils (classNames)
import TodoMVC.Components.TodoTextInput (todoTextInput)

todoItem :: R.ReactClass { todo :: Todo }
todoItem = accessContext $ R.createClass $ ((R.spec { editing: false } renderFn) { displayName = "TodoItem", shouldComponentUpdate = shouldComponentUpdate })
  where
    _todo this = R.getProps this >>= pure <<< unwrap <<< _.todo

    handleChange this ev = do
      todo <- _todo this
      dispatch this (completeTodo todo.id)

    handleDoubleClick this ev = do
      R.transformState this (_ { editing = true })

    handleSave this text = do
      todo <- _todo this
      if todo.text == ""
        then void $ dispatch this (deleteTodo todo.id)
        else R.transformState this (_ { editing = false })

    handleDelete this ev = do
      todo <- _todo this
      void $ dispatch this (deleteTodo todo.id)

    shouldComponentUpdate this props state = do
      props_ <- R.getProps this
      pure $ props_.todo /= props.todo

    renderFn this = do
      -- note:
      -- if I use `_todo this` here then the script does not compile due to
      -- a type error: most likely because of multiple `read :: Read` in `eff`.
      todo <- unwrap <<< _.todo <$> R.getProps this
      editing <- _.editing <$> R.readState this
      let el =
            if editing
              then
                R.createElement todoTextInput
                  { text: todo.text
                  , editing
                  , onSave: handleSave this
                  , newTodo: false
                  , placeholder: "What's to be done?" 
                  } []
              else R.div [ P.className "view" ]
                [ R.input [ P.className "toggle"
                          , P._type "checkbox"
                          , P.checked todo.completed
                          , P.onChange $ handleChange this
                          ] []
                , R.label [ P.onDoubleClick $ handleDoubleClick this ] [ R.text todo.text ]
                , R.button [ P.className "destroy", P.onClick $ handleDelete this ] []
                ]
      pure $ R.li
        [ classNames [ Tuple "completed" todo.completed, Tuple "editing" editing ]
        , P.key $ show todo.id ]
        [ el ]

