module Levity.Class.Foldable
  ( --Foldable(..)
  ) where

import qualified Data.Foldable as Foldable
import GHC.Prim
import GHC.Types

import qualified Prelude

import Levity.Types.Base
import Levity.Class.Monoid

{-
class FoldableL (f :: Type -> TYPE r) where
  {-# minimal foldMapL | foldrL #-}
  foldMapL :: MonoidL m => (a -> m) -> f a -> m
  foldrL :: forall (b :: TYPE r). (a -> b -> b) -> b -> f a -> b

  foldMapL f = foldrL (appendL . f) (mempty ())

  foldr f z t = appEndoL (foldMapL (EndoL . f) t) z
-}

--composeL :: forall (a :: TYPE r) (b :: TYPE r) (c :: TYPE r).
--  (b -> c) -> (a -> b) -> (a -> c)
--composeL = _ --f (g x)

--newtype EndoL (a :: TYPE r) = EndoL { appEndo :: a -> a }
--
--instance SemigroupL (EndoL a) where

{-
instance Prelude.Foldable f => Foldable (f :: Type -> Type) where
  fold = Foldable.fold

instance Foldable ((# , #) a) where
  fold (# _, m #) = m
-}
