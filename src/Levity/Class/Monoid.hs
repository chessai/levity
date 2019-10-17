module Levity.Class.Monoid
  ( MonoidL(..)
  ) where

import GHC.Base hiding (Monoid)
import GHC.Prim
import Prelude ()

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Semigroup

class SemigroupL a => MonoidL a where
  {-# minimal memptyL #-}
  memptyL :: () -> a

instance Prelude.Monoid a => MonoidL (Base a) where
  memptyL = const Prelude.mempty
