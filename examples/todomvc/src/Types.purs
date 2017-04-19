module TodoMVC.Types 
  ( State
  , Todo(..)
  , Filter(..)
  ) where

import Prelude (class Eq, class Show)
import Data.Newtype (class Newtype)

type State = Array Todo

newtype Todo = Todo
  { text :: String
  , completed :: Boolean
  , id :: Int
  }

derive instance newtypeTodo :: Newtype Todo _

foreign import eqTodo_ :: Todo -> Todo -> Boolean

instance eqTodo :: Eq Todo where
  eq td1 td2 = eqTodo_ td1 td2

data Filter = All | Active | Completed

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

derive instance eqFilter :: Eq Filter
