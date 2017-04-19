module TodoMVC.Components.Footer where

import Prelude
import Data.Array (singleton)
import React as R
import React (ReactClass, Event, EventHandlerContext)
import React.DOM as R
import React.DOM.Props as P

import TodoMVC.Types (Filter(..))

footer :: ReactClass
  { completedCount :: Int
  , activeCount :: Int
  , filter :: Filter
  , onShow :: Filter -> Event -> EventHandlerContext _ _ Unit Unit
  , onClearCompleted :: Event -> EventHandlerContext _ _ Unit Unit
  }
footer = R.createClass ((R.spec unit renderFn) { displayName = "Footer" })
  where
    renderTodoCount props = 
      let activeCount = props.activeCount
          itemWord = if activeCount == 1 then "item" else "items"
      in R.span [ P.className "todo-count" ]
          [ R.strong' [ R.text $ if activeCount > 0 then show activeCount else "No" ]
          , R.text $ " " <>  itemWord <> " left"
          ]

    renderFilterLink filter props =
      R.a [ P.className $ if filter == props.filter then "selected" else ""
          , P.style { cursor: "pointer" }
          , P.onClick $ props.onShow filter
          ]
        [ R.text $ show filter ]

    renderClearButton props =
      let completedCount = props.completedCount
          onClearCompleted = props.onClearCompleted
      in R.button [ P.className "clear-completed", P.onClick onClearCompleted ] [ R.text "Clear completed" ]

    renderFn this = do
      props <- R.getProps this
      pure $ R.footer [ P.className "footer" ]
              [ renderTodoCount props
              , R.ul [ P.className "filters" ] $
                  flip map [All, Active, Completed] (\filter -> R.li [ P.key $ show filter ] $ singleton $ renderFilterLink filter props)
              , renderClearButton props
              ]
