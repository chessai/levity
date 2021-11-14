module Levity.Class.Show
  ( ShowL(..)
  ) where

import GHC.Base
import GHC.Prim
import Prelude qualified

import Levity.Types.Base

class ShowL (a :: TYPE r) where
  {-# minimal showL #-}
  showL :: a -> String

instance Prelude.Show a => ShowL (Base a) where
  showL = Prelude.show
