module Levity.Class.Eq
  ( EqL(..)
  ) where

import Data.Bool (Bool)
import GHC.Exts

import Prelude qualified

import Levity.Types.Base

type EqL :: TYPE r -> Constraint
class EqL a where
  {-# minimal eqL, neqL #-}
  eqL :: a -> a -> Bool

  -- Unfortunately this is necessarily apart of the class because of binder restrictions.
  neqL :: a -> a -> Bool

instance Prelude.Eq a => EqL (Base a) where
  eqL = (Prelude.==)
  neqL = (Prelude./=)
