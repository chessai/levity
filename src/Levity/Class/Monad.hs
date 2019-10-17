module Levity.Class.Monad
  ( MonadL(..)
  ) where

import GHC.Prim
import GHC.Types

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Applicative

class ApplicativeL f => MonadL (f :: Type -> TYPE r) where
  {-# minimal bindL #-}
  bindL :: f a -> (a -> f b) -> f b

instance Prelude.Monad f => MonadL (Base1 f) where
  bindL = (Prelude.>>=)
