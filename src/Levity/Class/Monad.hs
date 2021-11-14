module Levity.Class.Monad
  ( MonadL(..)
  ) where

import GHC.Prim
import GHC.Types

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Applicative

type MonadL :: (Type -> TYPE r) -> Constraint
class ApplyL f => MonadL f where
  bindL :: f a -> (a -> f b) -> f b

instance Prelude.Monad f => MonadL (Base1 f) where
  bindL = (Prelude.>>=)
