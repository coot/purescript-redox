module TodoMVC.Store where

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Foldable (all, foldl)
import Data.List (List(..), filter, snoc, (:))
import Data.Newtype (over, un)
import Prelude (class Functor, bind, id, map, max, not, otherwise, pure, ($), (+), (/=), (<$>), (<<<), (==), (>>=), (>>>))
import Redox (Store, getState)
import Redox.Store (ReadRedox, RedoxStore)
import TodoMVC.Action (Action(ClearCompleted, CompleteAll, CompleteTodo, EditTodo, DeleteTodo, AddTodo), ActionDSL)
import TodoMVC.Types (Todo(..), State)

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
initialState =
  (Todo
    {
      text: "Use Redox",
      completed: false,
      id: 0
    })
  : Nil

mkInterp
  :: forall rx e eff
   . Store State
  -> Eff
      ( redox :: RedoxStore (read :: ReadRedox | rx )
      | e )
      (Cofree (Run (redox :: RedoxStore (read :: ReadRedox | rx) | eff)) State)
mkInterp store = getState store >>= unfoldCofree id next >>> pure
  where
    addTodo :: String -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) State
    addTodo text = do
      s <- getState store
      pure $ snoc s (Todo
        { text
        , completed: false
        , id: (foldl (\a td -> max a $ (un Todo td).id) 0 s) + 1
        })

    deleteTodo :: Int -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) State
    deleteTodo _id = do
      s <- getState store
      pure $ filter ((_id /= _) <<< _.id <<< un Todo) s

    editTodo :: Int -> String -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) State
    editTodo _id text = do
      s <- getState store
      pure $ _editTodo _id text s

    _editTodo :: Int -> String -> State -> State
    _editTodo _ _ Nil = Nil
    _editTodo _id text ((Todo t) : ts)
      | _id == t.id = Todo (t { text = text }) : ts
      | otherwise   = Todo t : _editTodo _id text ts

    completeTodo :: Int -> Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) State
    completeTodo _id = _toggleTodo _id <$> getState store

    _toggleTodo :: Int -> State -> State
    _toggleTodo _ Nil = Nil
    _toggleTodo _id ((Todo t) : ts)
      | _id == t.id = Todo (t { completed = not t.completed }) : ts
      | otherwise   = Todo t : _toggleTodo _id ts

    completeAll :: Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) State
    completeAll = do
      s <- getState store
      let allMarked = all (_.completed <<< un Todo) s
      pure $ map (over Todo (_ { completed = not allMarked })) s

    clearCompleted :: Aff (redox :: RedoxStore (read :: ReadRedox | rx) | eff) State
    clearCompleted =
      filter (not <<< _.completed <<< un Todo) <$> getState store

    next state_ = Run
      { addTodo
      , deleteTodo
      , editTodo
      , completeTodo
      , completeAll
      , clearCompleted
      }

pair :: forall x y eff. Action (x -> y) -> Run eff x -> Aff eff y
pair (AddTodo text f) (Run interp) = f <$> interp.addTodo text
pair (DeleteTodo id f) (Run interp) = f <$> interp.deleteTodo id
pair (EditTodo id text f) (Run interp) = f <$> interp.editTodo id text
pair (CompleteTodo id f) (Run interp) = f <$> interp.completeTodo id
pair (CompleteAll f) (Run interp) = f <$> interp.completeAll
pair (ClearCompleted f) (Run interp) = f <$> interp.clearCompleted

runAction
  :: forall eff
   . Cofree (Run eff) State
  -> ActionDSL (State -> State)
  -> State
  -> Aff eff State
runAction interp cmds state = exploreM pair cmds interp
