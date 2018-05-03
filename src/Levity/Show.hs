{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Show
  ( Show(..)
  ) where

import GHC.Base
import GHC.Prim
import Prelude (String)

import qualified Prelude

class Show (a :: TYPE (k :: RuntimeRep)) where
  {-# MINIMAL show #-}
  show :: a -> String

instance Prelude.Show a => Show (a :: Type) where
  show = Prelude.show

