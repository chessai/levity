module Levity.Class.Semigroup
  ( SemigroupL(..)
  ) where

import GHC.Base hiding (Semigroup)
import GHC.Prim
import Prelude ()

import qualified Prelude

import Levity.Types.Base

class SemigroupL (a :: TYPE r) where
  {-# minimal appendL #-}
  appendL :: a -> a -> a

instance Prelude.Semigroup a => SemigroupL (Base a) where
  appendL = (Prelude.<>)
