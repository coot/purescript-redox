module TodoMVC.Types where

import Data.Newtype (class Newtype)

newtype Todo = Todo
  { text :: String
  , completed :: Boolean
  , id :: Int
  }

derive instance newtypeTodo :: Newtype Todo _
