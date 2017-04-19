module TodoMVC.Components.TodoTextInput where

import Prelude
import Data.Array
import Data.Maybe
import React as R
import React.DOM as R
import React.DOM.Props as P
import Control.Monad.Eff.Console (log)
import DOM.Event.Event (target)
import DOM.HTML.HTMLInputElement (setValue, value)
import DOM.HTML.Types (HTMLInputElement)
import DOM.Node.Node (nodeValue)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import TodoMVC.Utils (classNames)
import Unsafe.Coerce (unsafeCoerce)

todoTextInput :: R.ReactClass
  { onSave :: String -> R.EventHandlerContext _ _ _ Unit
  , text :: String
  , placeholder :: String
  , editing :: Boolean
  , newTodo :: Boolean
  }
todoTextInput = R.createClass $ ((R.spec { text: "" } renderFn) { displayName = "TodoTextInput" })
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
      void $ R.writeState this { text }

    handleSubmit this props ev = do
      let tgElement = unsafeCoerce $ target $ unsafeCoerce ev
      text <- value $ tgElement
      if (ev.which == 13)
        then do
          props.onSave text
          setValue "" tgElement
          if props.newTodo
            then void $ R.writeState this { text: "" }
            else pure unit
        else pure unit

    renderFn this = do
      props <- R.getProps this
      pure $ R.input
        [ classNames [ Tuple "edit" props.editing, Tuple "new-todo" props.newTodo ]
        , P.placeholder props.placeholder
        , P.autoFocus true
        , P.onBlur $ handleBlur props
        , P.onChange $ handleChange this props
        , P.onKeyDown $ handleSubmit this props
        ] []
