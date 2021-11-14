module Levity.Class.Ord
  ( OrdL(..)
  ) where

import GHC.Base (Ordering)
import GHC.Exts
import Prelude qualified

import Levity.Types.Base
import Levity.Class.Eq (EqL(..))

type OrdL :: TYPE r -> Constraint
class EqL a => OrdL a where
  {-# minimal compareL #-}
  compareL :: a -> a -> Ordering

instance Prelude.Ord a => OrdL (Base a) where
  compareL = Prelude.compare
