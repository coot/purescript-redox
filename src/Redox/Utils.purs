module Redox.Utils where

import Prelude
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<), head, tail)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Redox.Store (Store, setState, performRedoxEff)
import Unsafe.Coerce (unsafeCoerce)

-- | A version of `hoistCofree`, where `nat` does not need to come from natural
-- | transformation.
-- | This is equivalent of
-- | [`applyMiddleware`](https://github.com/reactjs/redux/blob/master/src/applyMiddleware.js) 
-- | from the redux library.
hoistCofree'
  :: forall f state
   . (Functor f)
  => (f (Cofree f state) -> f (Cofree f state))
  -> Cofree f state
  -> Cofree f state
hoistCofree' nat cf = head cf :< nat (hoistCofree' nat <$> tail cf)

-- | Make interpreter which updates the store on every step of computation.
-- | You have to supply the store and interperter of type `Cofree f state`.
-- | Check out test how you can use it.
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
    g cof = performRedoxEff do
      setState store (head cof)
      pure cof
