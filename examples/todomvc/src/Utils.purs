module TodoMVC.Utils where

import Prelude
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import React.DOM.Props (Props, className)

classNames :: Array (Tuple String Boolean) -> Props
classNames cssClasses = className $ foldl (\acu (Tuple clsName b) -> if b then acu <> " " <> clsName else acu) "" cssClasses
