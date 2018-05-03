{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Monoid
  ( Monoid(..)
  ) where

import GHC.Base hiding (Semigroup, Monoid)
import GHC.Prim

import Prelude ()
import qualified Prelude

import Levity.Semigroup (Semigroup(..))

class Semigroup (a :: TYPE (k :: RuntimeRep))
  => Monoid (a :: TYPE (k :: RuntimeRep)) where
    {-# MINIMAL mempty #-}
    mempty :: () -> a

instance Prelude.Monoid a => Monoid (a :: Type) where
  mempty = Prelude.mempty

