{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Foldable
  ( Foldable(..)
  ) where

import qualified Data.Foldable as Foldable
import GHC.Prim
import GHC.Types
import Prelude (Monoid(..))

import qualified Prelude

class Foldable (f :: Type -> TYPE (k :: RuntimeRep)) where
  {-# MINIMAL fold #-}
  fold :: Monoid m => f m -> m 

instance Prelude.Foldable f => Foldable (f :: Type -> Type) where
  fold = Foldable.fold

instance Foldable ((# , #) a) where
  fold (# _, m #) = m

