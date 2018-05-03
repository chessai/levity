{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Functor
  ( Functor(..)
  ) where

import GHC.Prim
import GHC.Types
import Prelude (String)

import qualified Prelude

class Functor (f :: Type -> TYPE (k :: RuntimeRep)) where
  {-# MINIMAL fmap #-}
  fmap :: (a -> b) -> (f a -> f b)

instance Prelude.Functor f => Functor (f :: Type -> Type) where
  fmap = Prelude.fmap

instance Functor ((# , #) a) where
  fmap f (# a, b #) = (# a, f b #)

