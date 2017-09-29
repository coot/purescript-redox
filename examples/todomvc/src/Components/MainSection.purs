module TodoMVC.Components.MainSection where

import Prelude

import Data.Lens (to)
import Data.List (List(..), filter, length, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import React (ReactClass, ReactElement, createClass, createElement, getProps, readState, writeState, spec)
import React.DOM as R
import React.DOM.Props as P
import React.Redox (connect, dispatch)
import TodoMVC.Action (clearCompleted, completeAll)
import TodoMVC.Components.Footer (footer)
import TodoMVC.Components.TodoItem (todoItem)
import TodoMVC.Types (State, Todo, Filter(..))
import Type.Proxy (Proxy(..))

mainSection :: ReactClass Unit
mainSection = connect
  (Proxy :: Proxy State)
  (to id)
  (\_ todos _ -> { todos })
  $ createClass $ ((spec { filter: All } renderFn) { displayName = "MainSection" })
    where
      handleShow this fltr ev =
        void $ writeState this { filter: fltr }

      handleClearCompleted this ev =
        void $ dispatch this clearCompleted

      renderToggleAll this allCount completedCount =
        if allCount > 0
          then
            [ R.input
                [ P.className "toggle-all"
                , P._type "checkbox"
                , P.checked $ completedCount == allCount
                , P.onChange \_ -> dispatch this completeAll
                ]
                []
            ]
          else []

      renderFooter this todos fltr allCount activeCount completedCount =
        if allCount > 0
          then [ createElement footer
                  { completedCount
                  , activeCount
                  , filter: fltr
                  , onShow: handleShow this
                  , onClearCompleted: handleClearCompleted this
                  }
                  []
               ]
          else []

      renderTodo :: List Todo -> Maybe (Tuple ReactElement (List Todo))
      renderTodo Nil = Nothing
      renderTodo (t : ts) = Just (Tuple (createElement todoItem { todo: t } []) ts)

      renderFn this = do
        todos <- getProps this >>= pure <<< _.todos
        fltr <- readState this >>= pure <<< _.filter
        let
            allCount = length todos
            completedCount = length $ filter (_.completed <<< unwrap) todos
            activeCount = length todos - completedCount

            filteredTodos :: List Todo
            filteredTodos = case fltr of
              All -> todos
              Active -> filter (not <<< _.completed <<< unwrap) todos
              Completed -> filter (_.completed <<< unwrap) todos
        pure $ R.section [ P.className "main" ]
          ( renderToggleAll this allCount completedCount
            <> [ R.ul [ P.className "todo-list" ] (unfoldr renderTodo filteredTodos) ]
            <> renderFooter this todos fltr allCount activeCount completedCount
          )

