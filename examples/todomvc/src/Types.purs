module TodoMVC.Types 
  ( State
  , Todo(..)
  , Filter(..)
  ) where

import Data.List (List)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Show)

type State = List Todo

newtype Todo = Todo
  { id :: Int
  , text :: String
  , completed :: Boolean
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
