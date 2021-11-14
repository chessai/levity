module Levity.Class.Monoid
  ( MonoidL(..)
  ) where

import GHC.Exts
import Prelude qualified

import Levity.Types.Base
import Levity.Class.Semigroup

type MonoidL :: TYPE r -> Constraint
class SemigroupL a => MonoidL a where
  {-# minimal memptyL #-}
  memptyL :: () -> a

instance Prelude.Monoid a => MonoidL (Base a) where
  memptyL () = Prelude.mempty
