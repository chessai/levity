module Levity.Class.Semigroup
  ( SemigroupL(..)
  ) where

import GHC.Exts
import Prelude qualified

import Levity.Types.Base

type SemigroupL :: TYPE r -> Constraint
class SemigroupL a where
  {-# minimal appendL #-}
  appendL :: a -> a -> a

instance Prelude.Semigroup a => SemigroupL (Base a) where
  appendL = (Prelude.<>)
