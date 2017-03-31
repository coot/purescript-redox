module Redox.Utils where

import Prelude
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<), head, tail)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Redox.Store (Store, setState, performRedoxEff)

-- | Make interpreter which updates the store on every step of computation.
-- | You have to supply the store and interperter of type `Cofree f state`.
-- | Check out test how you can use it.
mkIncInterp
  :: forall state f
   . (Functor f)
  => Store state
  -> Cofree f state
  -> Cofree f state
mkIncInterp store interp = hoist nat interp
  where
    -- like hoistCofree, but we don't have natural transformation here
    hoist nat cf = head cf :< nat (hoist nat <$> tail cf)

    nat
      :: f (Cofree f state)
      -> f (Cofree f state)
    nat fa = g <$> fa

    g :: Cofree f state -> Cofree f state
    g cof = performRedoxEff do
      setState store (head cof)
      pure cof

