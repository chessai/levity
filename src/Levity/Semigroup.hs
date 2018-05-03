{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Semigroup
  ( Semigroup(..)
  ) where

import GHC.Base hiding (Semigroup)
import GHC.Prim
import Prelude ()

import qualified Prelude

class Semigroup (a :: TYPE (k :: RuntimeRep)) where
    {-# MINIMAL (<>) #-}

    (<>) :: a -> a -> a

instance Prelude.Semigroup a => Semigroup (a :: Type) where
  (<>) = (Prelude.<>)  

