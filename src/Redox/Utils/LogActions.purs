module Redox.Utils.LogActions
  ( class LogActions
  , logActions
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (log) as Eff
import Data.Maybe (fromJust)
import Data.StrMap as SM
import Data.Symbol (SProxy(..))
import Partial.Unsafe (unsafePartial)
import Prelude ((*>), (<<<))
import Type.Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Row (class ListToRow, class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

unsafeGet
  :: forall r a
   . String
  -> Record r
  -> a
unsafeGet s = unsafePartial fromJust <<< SM.lookup s <<< unsafeCoerce

unsafeSet
  :: forall r1 r2 a
   . String
  -> a
  -> Record r1
  -> Record r2
unsafeSet s a = unsafeCoerce <<< SM.insert s a <<< unsafeCoerce

class LogActions (il :: RowList) (ol :: RowList) where
  logActions
    :: forall ir or
     . RowToList ir il
    => ListToRow il ir
    => RowToList or ol
    => ListToRow ol or
    => Record ir
    -> Record or

instance logActionsNil :: LogActions Nil Nil where
  logActions = unsafeCoerce

instance logActionsAffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
  => LogActions (Cons name (Aff ei a) ti) (Cons name (Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (log n *> unsafeGet n r) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsEffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
  => LogActions (Cons name (Eff e a) ti) (Cons name (Eff e a) to) where
  logActions r = unsafeCoerce (unsafeSet n (Eff.log n *> unsafeGet n r) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF1AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
  => LogActions (Cons name (x -> Aff e a) ti) (Cons name (x -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x -> log n *> unsafeGet n r x) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF2AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
  => LogActions (Cons name (x1 -> x2 -> Aff e a) ti) (Cons name (x1 -> x2 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 -> log n *> unsafeGet n r x1 x2) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF3AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
  => LogActions (Cons name (x1 -> x2 -> x3 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 -> log n *> unsafeGet n r x1 x2 x3) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF4AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 -> log n *> unsafeGet n r x1 x2 x3 x4) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF5AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 x5 -> log n *> unsafeGet n r x1 x2 x3 x4 x5) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF6AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 x5 x6 -> log n *> unsafeGet n r x1 x2 x3 x4 x5 x6) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF7AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 x5 x6 x7 -> log n *> unsafeGet n r x1 x2 x3 x4 x5 x6 x7) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF8AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 x5 x6 x7 x8 -> log n *> unsafeGet n r x1 x2 x3 x4 x5 x6 x7 x8) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF9AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 x5 x6 x7 x8 x9 -> log n *> unsafeGet n r x1 x2 x3 x4 x5 x6 x7 x8 x9) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)

instance logActionsF10AffCons
  :: ( IsSymbol name
     , LogActions ti to
     , RowToList tir ti
     , ListToRow ti tir
     , RowToList tor to
     , ListToRow to tor
     )
     => LogActions (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> Aff e a) ti) (Cons name (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> Aff eo a) to) where
  logActions r = unsafeCoerce (unsafeSet n (\x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 -> log n *> unsafeGet n r x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) tor)
    where
      n :: String
      n = reflectSymbol (SProxy :: SProxy name)
      tor :: Record tor
      tor = logActions (unsafeCoerce r :: Record tir)
