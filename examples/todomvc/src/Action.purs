module TodoMVC.Action where

import Prelude
import Control.Monad.Free (Free, liftF)
import TodoMVC.Types (State)

data Action a 
  = AddTodo String a
  | DeleteTodo Int a
  | EditTodo Int String a
  | CompleteTodo Int a
  | CompleteAll a
  | ClearCompleted a

derive instance functorAction :: Functor Action

type ActionDSL a = Free Action a

addTodo :: String -> ActionDSL (State -> State)
addTodo td = do
  liftF $ AddTodo td unit
  pure id

deleteTodo :: Int -> ActionDSL (State -> State)
deleteTodo id_ = do
  liftF $ DeleteTodo id_ unit
  pure id

editTodo :: Int -> String -> ActionDSL (State -> State)
editTodo id_ td = do
  liftF $ EditTodo id_ td unit
  pure id

completeTodo :: Int -> ActionDSL (State -> State)
completeTodo id_ = do
  liftF $ CompleteTodo id_ unit
  pure id

completeAll :: ActionDSL (State -> State)
completeAll = do
  liftF $ CompleteAll unit
  pure id

clearCompleted :: ActionDSL (State -> State)
clearCompleted = do
  liftF $ ClearCompleted unit
  pure id
