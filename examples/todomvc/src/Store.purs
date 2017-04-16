module TodoMVC.Store where

import Prelude (class Functor, id, map, max, not, ($), (+), (/=), (<<<), (==))
import Data.Array (snoc, filter)
import Data.Newtype (unwrap, over)
import Data.Foldable (all, foldl)
import Control.Comonad.Cofree (Cofree, explore, unfoldCofree)

import TodoMVC.Types (Todo(..))
import TodoMVC.Action (Action(..), ActionDSL)

newtype Run a = Run
  { addTodo :: String -> a
  , deleteTodo :: Int -> a
  , editTodo :: Int -> String -> a
  , completeTodo :: Int -> a
  , completeAll :: a
  , clearCompleted :: a
  }

derive instance functorRun :: Functor Run

type Interp a = Cofree Run a

mkInterp :: Array Todo -> Interp (Array Todo)
mkInterp state = unfoldCofree id next state
  where
    updateTodoText :: Int -> String -> Array Todo -> Todo -> Array Todo
    updateTodoText id text acu td = 
      if id == (unwrap td).id
        then snoc acu (over Todo (_ { text = text }) td)
        else acu

    toggleTodo :: Int -> Array Todo -> Todo -> Array Todo
    toggleTodo id acu td = 
      if id == (unwrap td).id
        then
          let completed = not (unwrap td).completed
          in snoc acu $ over Todo (_ { completed = not completed }) td
        else snoc acu td

    completeAll :: Array Todo -> Array Todo
    completeAll state_ =
      let areAllMarked = all (_.completed <<< unwrap) state_
      in map (over Todo (_ { completed = not areAllMarked })) state_

    next :: Array Todo -> Run (Array Todo)
    next state_ = Run
      { addTodo: \text -> snoc state_ (Todo {text, completed: false, id: (foldl (\a td -> max a $ (unwrap td).id) 0 state_) + 1 })
      , deleteTodo: \id -> filter ((id /= _) <<< _.id <<< unwrap) state_
      , editTodo: \id text -> foldl (updateTodoText id text) [] state_
      , completeTodo: \id -> foldl (toggleTodo id) [] state_
      , completeAll: completeAll state_
      , clearCompleted: filter (not <<< _.completed <<< unwrap) state_
      }

pair :: forall x y. Action (x -> y) -> Run x -> y
pair (AddTodo text f) (Run interp) = f $ interp.addTodo text
pair (DeleteTodo id f) (Run interp) = f $ interp.deleteTodo id
pair (EditTodo id text f) (Run interp) = f $ interp.editTodo id text
pair (CompleteTodo id f) (Run interp) = f $ interp.completeTodo id
pair (CompleteAll f) (Run interp) = f $ interp.completeAll
pair (ClearCompleted f) (Run interp) = f $ interp.clearCompleted

dispatchAction :: ActionDSL (Array Todo -> Array Todo) -> Array Todo-> Array Todo
dispatchAction cmds state = explore pair cmds $ mkInterp state
