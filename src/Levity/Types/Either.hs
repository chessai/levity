{-# language RankNTypes #-}

module Levity.Types.Either
  ( Either(..)
  , either
  ) where 

import "base" GHC.Exts
import "base" Data.Kind (Constraint, Type)
import "base" Prelude qualified 

type Either :: TYPE r1 -> TYPE r2 -> Type
newtype Either a b = Either (forall {ex :: RuntimeRep} (r :: TYPE ex). (a -> r) -> (b -> r) -> r)

{-
left :: forall {r1 :: RuntimeRep} {r2 :: RuntimeRep} (a :: TYPE r1) (b :: TYPE r2). ()
  => a
  -> Either a b
left a = Either (\l _r -> l a)

right :: forall {r1 :: RuntimeRep} {r2 :: RuntimeRep} (a :: TYPE r1) (b :: TYPE r2). ()
  => b
  -> Either a b
right b = Either (\_l r -> r b)
-}

either :: forall {r1 :: RuntimeRep} {r2 :: RuntimeRep} {r3 :: RuntimeRep} (a :: TYPE r1) (b :: TYPE r2) (result :: TYPE r3). ()
  => (a -> result)
  -> (b -> result)
  -> Either a b
  -> result
either fa fb (Either e) = e fa fb
