{-# language RankNTypes #-}

module Levity.Types.Maybe
  ( Maybe(..)
  , nothing
  , maybe
  ) where

import "base" GHC.Exts
import "base" Data.Kind (Constraint, Type)
import "base" Prelude qualified 

type Maybe :: TYPE r -> Type
newtype Maybe a = Maybe (forall {ex :: RuntimeRep} (r :: TYPE ex). (() -> r) -> (a -> r) -> r)

{-
just :: forall {r :: RuntimeRep} (a :: TYPE r). ()
  => (() -> a)
  -> Maybe a
just a = Maybe (\_n j -> j (a ()))
-}

nothing :: forall {r :: RuntimeRep} (a :: TYPE r). ()
  => Maybe a
nothing = Maybe (\n _j -> n ())

maybe :: forall {r1 :: RuntimeRep} {r2 :: RuntimeRep} (a :: TYPE r1) (b :: TYPE r2). ()
  => (() -> b)
  -> (a -> b)
  -> Maybe a
  -> b
maybe b f (Maybe m) = m b f 
