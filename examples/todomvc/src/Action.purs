module TodoMVC.Action where

import Prelude
import Control.Monad.Free (Free, liftF)

import TodoMVC.Types

data Action a 
  = AddTodo String a
  | DeleteTodo Int a
  | EditTodo Int String a
  | CompleteTodo Int a
  | CompleteAll a
  | ClearCompleted a

derive instance functorAction :: Functor Action

type ActionDSL a = Free Action a

addTodo :: String -> ActionDSL Unit
addTodo td = liftF $ AddTodo td unit

deleteTodo :: Int -> ActionDSL Unit
deleteTodo id = liftF $ DeleteTodo id unit

editTodo :: Int -> String -> ActionDSL Unit
editTodo id td = liftF $ EditTodo id td unit

completeTodo :: Int -> ActionDSL Unit
completeTodo id = liftF $ CompleteTodo id unit

completeAll :: ActionDSL Unit
completeAll = liftF $ CompleteAll unit

clearCompleted :: ActionDSL Unit
clearCompleted = liftF $ ClearCompleted unit
