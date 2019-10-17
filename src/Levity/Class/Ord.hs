module Levity.Class.Ord
  ( OrdL(..)
  ) where

import GHC.Base (Ordering)
import GHC.Prim
import Prelude ()

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Eq (EqL(..))

class EqL a => OrdL a where
  {-# minimal compareL #-}
  compareL :: a -> a -> Ordering

instance Prelude.Ord a => OrdL (Base a) where
  compareL = Prelude.compare
