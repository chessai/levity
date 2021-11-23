module Levity.Types.IO
  ( IO(..)
  ) where

import "base" GHC.Exts
import "base" Data.Kind (Type)
import "base" Prelude qualified

import Levity.Types.ST qualified as STL
import Levity.Class.Functor
import Levity.Class.Applicative
import Levity.Class.Monad

type IO :: TYPE r -> Type
newtype IO a = IO (STL.ST RealWorld a)
  deriving newtype (Prelude.Functor, Prelude.Applicative, Prelude.Monad)
  deriving newtype (FunctorL, ApplyL, ApplicativeL, MonadL)

type role IO representational
