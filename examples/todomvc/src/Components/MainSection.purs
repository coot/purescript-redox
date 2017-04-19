module TodoMVC.Components.MainSection where

import Prelude
import React as R
import React.DOM as R
import React.DOM.Props as P
import Data.Array (filter)
import Data.Array (length)
import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import React.Redox (connect, dispatch, withStore)
import ReactHocs (accessContext)
import TodoMVC.Action (clearCompleted, completeAll)
import TodoMVC.Components.Footer (footer)
import TodoMVC.Components.Header (header)
import TodoMVC.Components.TodoItem (todoItem)
import TodoMVC.Store (store)
import TodoMVC.Types (State, Todo, Filter(..))
import TodoMVC.Utils (classNames)

_lens :: Lens' State State
_lens = lens id (flip const)

mainSection :: R.ReactClass Unit
mainSection = connect _lens (\todos _ -> { todos }) $ R.createClass $ ((R.spec { filter: All } renderFn) { displayName = "MainSection" })
    where
      handleShow this fltr ev =
        void $ R.writeState this { filter: fltr }

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
          then [ R.createElement footer
                  { completedCount
                  , activeCount
                  , filter: fltr
                  , onShow: handleShow this
                  , onClearCompleted: handleClearCompleted this
                  }
                  []
               ]
          else []

      renderTodo :: Todo -> R.ReactElement
      renderTodo td =
        let td_ = unwrap td
        in R.createElement todoItem { todo: td } []

      renderFn this = do
        todos <- R.getProps this >>= pure <<< _.todos
        fltr <- R.readState this >>= pure <<< _.filter
        let
            allCount = length todos
            completedCount = length $ filter (_.completed <<< unwrap) todos
            activeCount = length todos - completedCount

            filteredTodos :: Array Todo
            filteredTodos = case fltr of
                              All -> todos
                              Active -> filter (not <<< _.completed <<< unwrap) todos
                              Completed -> filter (_.completed <<< unwrap) todos
        pure $ R.section [ P.className "main" ]
          ( renderToggleAll this allCount completedCount
            <> [ R.ul [ P.className "todo-list" ] (map renderTodo filteredTodos) ]
            <> renderFooter this todos fltr allCount activeCount completedCount
          )

