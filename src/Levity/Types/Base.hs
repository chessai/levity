{-# language
    DeriveTraversable
  , DerivingStrategies
  , DerivingVia
  , GeneralizedNewtypeDeriving
  #-}

module Levity.Types.Base
  ( Base(..)
  , Base1(..)
  , Base2(..)
  ) where

import Data.Bits (Bits)
import Data.Bifoldable (Bifoldable)
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Bifunctor (Bifunctor)
import Data.Functor.Contravariant (Contravariant)
import Data.String (IsString)
import Foreign.Storable (Storable)

newtype Base a = Base { getBase :: a }
  deriving newtype (Eq, Ord)
  deriving newtype (Show, Read)
  deriving newtype (Enum, Bounded)
  deriving newtype (Semigroup, Monoid)
  deriving newtype (Num, Real, Integral)
  deriving newtype (Fractional, Floating, RealFrac, RealFloat)
  deriving newtype (Bits)
  deriving newtype (IsString)
  deriving newtype (Storable)
--  deriving newtype (Prim)

newtype Base1 f a = Base1 { getBase1 :: f a }
  deriving newtype (Functor,Applicative,Monad)
  deriving newtype (Contravariant, Foldable)

instance Traversable f => Traversable (Base1 f) where
  traverse f (Base1 x) = Base1 <$> traverse f x

newtype Base2 f a b = Base2 { getBase2 :: f a b }
  deriving newtype (Bifunctor, Bifoldable)

instance Bitraversable f => Bitraversable (Base2 f) where
  bitraverse f g (Base2 x) = Base2 <$> bitraverse f g x
