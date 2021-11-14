module Levity.Class.Applicative
  ( ApplyL(..)
  , ApplicativeL(..)
  ) where

import GHC.Prim
import GHC.Types

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Functor

type ApplyL :: (Type -> TYPE r) -> Constraint
class FunctorL f => ApplyL f where
  apL :: f (a -> b) -> (f a -> f b)

type ApplicativeL :: (Type -> TYPE r) -> Constraint
class ApplyL f => ApplicativeL f where
  pureL :: a -> f a

instance Prelude.Applicative f => ApplyL (Base1 f) where
  apL = (Prelude.<*>)

instance Prelude.Applicative f => ApplicativeL (Base1 f) where
  pureL = Prelude.pure
