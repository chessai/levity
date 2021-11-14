{-# language NoImplicitPrelude #-}

module Levity
  ( Base(..)

  , EqL(..)
  , OrdL(..)
  , Add(..), (+)
  , Sub(..), (-)
  , Mul(..), (*)
  , Neg(..)
  , Signed(..)
  , ShowL(..)
  , SemigroupL(..)
  , MonoidL(..)

  , FunctorL(..)
  , ApplicativeL(..)
  , MonadL(..)
  , FoldableL(..)
  ) where

import Levity.Class.Eq
import Levity.Class.Ord
import Levity.Class.Num
import Levity.Class.Show
import Levity.Class.Semigroup
import Levity.Class.Monoid
import Levity.Class.Functor
import Levity.Class.Applicative
import Levity.Class.Monad
import Levity.Class.Foldable
import Levity.Types.Base
