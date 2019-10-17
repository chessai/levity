module Levity.Class.Show
  ( ShowL(..)
  ) where

import GHC.Base
import GHC.Prim
import Prelude (String)

import qualified Prelude

class ShowL (a :: TYPE r) where
  {-# minimal show #-}
  showL :: a -> String

