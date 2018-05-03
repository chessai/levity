{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Eq
  ( Eq(..)
  ) where

import GHC.Base hiding (Eq)
import GHC.Prim
import Prelude ()

import qualified Prelude

class Eq (a :: TYPE (k :: RuntimeRep)) where
  {-# MINIMAL (==) #-}
  (==) :: a -> a -> Bool

instance Prelude.Eq a => Eq (a :: Type) where
  (==) = (Prelude.==)  

