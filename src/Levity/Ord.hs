{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Ord
  ( Ord(..)
  , Ordering(..)
  ) where

import GHC.Base hiding (Eq, Ord)
import GHC.Prim
import Prelude ()

import qualified Prelude

import Levity.Eq (Eq(..))

class Eq (a :: TYPE (k :: RuntimeRep))
  => Ord (a :: TYPE (k :: RuntimeRep)) where
  {-# MINIMAL compare #-}
  compare :: a -> a -> Ordering

instance Prelude.Ord a => Ord (a :: Type) where
  compare = Prelude.compare 

