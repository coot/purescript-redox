module TodoMVC.Components.TodoTextInput where

import Prelude (Unit, bind, discard, not, pure, unit, void, ($), (==))
import React (EventHandlerContext, ReactClass, createClass, getProps, spec, writeState)
import React.DOM (input) as R
import React.DOM.Props as P
import DOM.Event.Event (target)
import DOM.HTML.HTMLInputElement (setValue, value)
import DOM.HTML.Types (HTMLInputElement)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import TodoMVC.Utils (classNames)
import Unsafe.Coerce (unsafeCoerce)

todoTextInput :: ReactClass
  { onSave :: String -> EventHandlerContext _ _ _ Unit
  , text :: String
  , placeholder :: String
  , editing :: Boolean
  , newTodo :: Boolean
  }
todoTextInput = createClass $ ((spec { text: "" } renderFn) { displayName = "TodoTextInput" })
  where
    handleBlur props ev =
      let node = target $ unsafeCoerce ev
          inputElement = unsafeCoerce node :: HTMLInputElement
      in do
        val <- value inputElement
        if not props.newTodo
          then do
            props.onSave $ trim val
          else pure unit

    handleChange this props ev = do
      text <- value $ unsafeCoerce $ target $ unsafeCoerce ev
      void $ writeState this { text }

    handleSubmit this props ev = do
      let tgElement = unsafeCoerce $ target $ unsafeCoerce ev
      text <- value $ tgElement
      if (ev.which == 13)
        then do
          props.onSave text
          setValue "" tgElement
          if props.newTodo
            then void $ writeState this { text: "" }
            else pure unit
        else pure unit

    renderFn this = do
      props <- getProps this
      pure $ R.input
        [ classNames [ Tuple "edit" props.editing, Tuple "new-todo" props.newTodo ]
        , P.placeholder props.placeholder
        , P.autoFocus true
        , P.onBlur $ handleBlur props
        , P.onChange $ handleChange this props
        , P.onKeyDown $ handleSubmit this props
        ] []
