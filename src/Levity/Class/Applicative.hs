module Levity.Class.Applicative
  ( ApplicativeL(..)
  ) where

import GHC.Prim
import GHC.Types

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Functor

class FunctorL f => ApplicativeL f where
  {-# minimal pureL, apL #-}
  pureL :: a -> f a
  apL :: f (a -> b) -> (f a -> f b)

instance Prelude.Applicative f => ApplicativeL (Base1 f) where
  pureL = Prelude.pure
  apL = (Prelude.<*>)
