module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId), documentToNonElementParentNode)
import Data.Traversable (traverse_)
import React (createElement)
import ReactDOM (render)
import Redox (CreateRedox, ReadRedox, RedoxStore, WriteRedox)
import TodoMVC.Components.App (app)

main
  :: forall rx eff
   . Eff
      (console :: CONSOLE, dom :: DOM, redox :: RedoxStore (create :: CreateRedox, read :: ReadRedox, write :: WriteRedox | rx) | eff)
      Unit
main = do
  app' <- app
  elm <- window
    >>= document
    >>= htmlDocumentToDocument
    >>> documentToNonElementParentNode
    >>> getElementById (ElementId "root")
  traverse_ (render $ createElement app' unit []) elm
