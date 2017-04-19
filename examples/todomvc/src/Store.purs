module TodoMVC.Store where

import Prelude
import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Aff (Aff)
import Data.Array (snoc, filter, mapWithIndex, replicate)
import Data.Foldable (all, foldl)
import Data.Newtype (unwrap, over)
import Prelude (class Functor, id, map, max, not, ($), (+), (/=), (<<<), (==))
import Redox (Store, mkStoreG)
import Redox.Utils (mkIncInterp)
import TodoMVC.Action (Action(..), ActionDSL)
import TodoMVC.Types (Todo(..), State)
import Global.Unsafe (unsafeStringify)

newtype Run eff a = Run
  { addTodo :: String -> Aff eff a
  , deleteTodo :: Int -> Aff eff a
  , editTodo :: Int -> String -> Aff eff a
  , completeTodo :: Int -> Aff eff a
  , completeAll :: Aff eff a
  , clearCompleted :: Aff eff a
  }

derive instance functorRun :: Functor (Run eff)

initialState :: State
initialState = [
  Todo
    {
      text: "Use Redox",
      completed: false,
      id: 0
    }
]
-- initialState :: State
-- initialState = mapWithIndex (\idx _ -> Todo { text: "todo: " <> show idx, completed: false, id: idx }) (replicate 100 0)

store :: Store State
store = mkStoreG initialState

type Interp eff a = Cofree (Run eff) a

mkInterp :: forall eff. State -> Interp eff State
mkInterp state = unfoldCofree id next state
  where
    updateTodoText :: Int -> String -> State -> Todo -> State
    updateTodoText id text acu td = 
      if id == (unwrap td).id
        then snoc acu (over Todo (_ { text = text }) td)
        else acu

    toggleTodo :: Int -> State -> Todo -> State
    toggleTodo id acu td = 
      if id == (unwrap td).id
        then
          let completed = not (unwrap td).completed
          in snoc acu $ over Todo (_ { completed = completed }) td
        else snoc acu td

    completeAll :: State -> State
    completeAll state_ =
      let areAllMarked = all (_.completed <<< unwrap) state_
      in map (over Todo (_ { completed = not areAllMarked })) state_

    next :: State -> Run eff (State)
    next state_ = Run
      { addTodo: \text -> pure $ snoc state_ (Todo {text, completed: false, id: (foldl (\a td -> max a $ (unwrap td).id) 0 state_) + 1 })
      , deleteTodo: \id -> pure $ filter ((id /= _) <<< _.id <<< unwrap) state_
      , editTodo: \id text -> pure $ foldl (updateTodoText id text) [] state_
      , completeTodo: \id -> do
          let newState = foldl (toggleTodo id) [] state_
          pure newState
      , completeAll: pure $ completeAll state_
      , clearCompleted: pure $ filter (not <<< _.completed <<< unwrap) state_
      }

pair :: forall x y eff. Action (x -> y) -> Run eff x -> Aff eff y
pair (AddTodo text f) (Run interp) = f <$> interp.addTodo text
pair (DeleteTodo id f) (Run interp) = f <$> interp.deleteTodo id
pair (EditTodo id text f) (Run interp) = f <$> interp.editTodo id text
pair (CompleteTodo id f) (Run interp) = f <$> interp.completeTodo id
pair (CompleteAll f) (Run interp) = f <$> interp.completeAll
pair (ClearCompleted f) (Run interp) = f <$> interp.clearCompleted

runAction :: forall eff. ActionDSL (State -> State) -> State-> Aff eff State
runAction cmds state = exploreM pair cmds $ mkInterp state
