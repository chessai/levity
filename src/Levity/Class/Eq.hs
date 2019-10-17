module Levity.Class.Eq
  ( EqL(..)
  , neqL
  ) where

import GHC.Base (Bool)
import GHC.Prim

import qualified Prelude

import Levity.Types.Base

class EqL (a :: TYPE r) where
  {-# minimal eqL #-}
  eqL :: a -> a -> Bool

neqL :: EqL a => a -> a -> Bool
neqL x y = Prelude.not (x `eqL` y)

instance Prelude.Eq a => EqL (Base a) where
  eqL = (Prelude.==)
