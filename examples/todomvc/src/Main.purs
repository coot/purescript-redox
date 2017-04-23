module Main where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, createElement)
import ReactDOM (render)
import TodoMVC.Components.App (app)

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void $ (elm' >>= render ui)
  where
    ui :: ReactElement
    ui = createElement app unit []

    elm' :: Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- document win
      elm <- getElementById (ElementId "root") (documentToNonElementParentNode $ htmlDocumentToDocument doc)
      pure $ unsafePartial $ fromJust elm
