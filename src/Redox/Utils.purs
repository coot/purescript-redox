module Redox.Utils where

import Prelude
import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Redox.Store as O
import Redox.Store (Store)

-- | A version of `hoistCofree`, where `nat` does not need to come from natural
-- | transformation.
-- | This is equivalent of
-- | [`applyMiddleware`](https://github.com/reactjs/redux/blob/master/src/applyMiddleware.js) 
-- | from the redux library.
-- | You can use this function to add effects to your interpreter, like
-- | logging, optimistic updates, undo/redo stack, delayed actions... 
-- | For example a simple logger:
-- | ```purescript
-- | addLogger
-- |   :: forall state f
-- |    . (Functor f)
-- |   => Cofree f state
-- |   -> Cofree f state
-- | addLogger interp = hoistCofree' nat interp
-- |   where
-- |     nat :: f (Cofree f state) -> f (Cofree f state)
-- |     nat fa = g <$> fa
-- | 
-- |     g :: Cofree f state -> Cofree f state
-- |     g cof = unsafePerformEff do
-- |       -- Control.Comonad.Cofree.head 
-- |       log $ unsafeCoerce (head cof)
-- |       pure cof
-- | ```purescript
hoistCofree'
  :: forall f state
   . (Functor f)
  => (f (Cofree f state) -> f (Cofree f state))
  -> Cofree f state
  -> Cofree f state
hoistCofree' nat cf = head cf :< nat (hoistCofree' nat <$> tail cf)

-- | Make interpreter which updates the store on every step of computation.
-- | You have to supply the store and interperter of type `Cofree f state`.
-- | Check out tests how you can use it.
mkIncInterp
  :: forall state f
   . (Functor f)
  => Store state
  -> Cofree f state
  -> Cofree f state
mkIncInterp store interp = hoistCofree' nat interp
  where
    nat
      :: f (Cofree f state)
      -> f (Cofree f state)
    nat fa = g <$> fa

    g :: Cofree f state -> Cofree f state
    g cof = O.performRedoxEff do
      O.setState store (head cof)
      pure cof
