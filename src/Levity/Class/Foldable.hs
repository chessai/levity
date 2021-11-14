module Levity.Class.Foldable
  ( FoldableL(..)
  ) where

import qualified Data.Foldable as Foldable
import GHC.Prim
import GHC.Types

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Monoid
import Levity.Class.Semigroup

type FoldableL :: (Type -> TYPE r) -> Constraint
class FoldableL f where
  foldMapL :: MonoidL m => (a -> m) -> f a -> m
  foldrL :: (a -> b -> b) -> b -> f a -> b

  foldMapL f = foldrL (\a m -> appendL (f a) m) (memptyL ())
  -- foldrL f z t = appEndoL (foldMapL (\a -> EndoL (f a)) t) z
