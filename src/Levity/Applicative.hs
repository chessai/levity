{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Applicative
  ( Applicative(..)
  ) where

import GHC.Prim
import GHC.Types
import Data.Semigroup ((<>))
import Prelude (Monoid(..))

import qualified Prelude

class Applicative (f :: Type -> TYPE (k :: RuntimeRep)) where
  {-# MINIMAL pure, (<*>) #-}
  pure :: a -> f a
  (<*>) :: f (a -> b) -> (f a -> f b)

instance Prelude.Applicative f => Applicative (f :: Type -> Type) where
  pure = Prelude.pure
  (<*>) = (Prelude.<*>)

instance Monoid m => Applicative ((# , #) m) where
  pure a = (# mempty, a #)
  (# m1, f #) <*> (# m2, x #) = (# m1 <> m2, f x #)
