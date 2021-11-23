{-# language
    BangPatterns
  , RankNTypes
  , RoleAnnotations
  , UnboxedTuples
  #-}

module Levity.Types.ST
  ( ST(..)
  , runST
  ) where

import GHC.Exts
import Data.Kind (Type)

import qualified Prelude
import qualified GHC.ST as ST

import Levity.Types.Base
import Levity.Class.Functor
import Levity.Class.Applicative
import Levity.Class.Monad

type ST :: Type -> TYPE r -> Type
newtype ST s a = ST (State# s -> (# State# s, a #))
  deriving (Prelude.Functor) via (ST.ST s)
  deriving (Prelude.Applicative) via (ST.ST s)
  deriving (Prelude.Monad) via (ST.ST s)
  deriving (FunctorL) via (Base1 (ST.ST s))
  deriving (ApplyL) via (Base1 (ST.ST s))
  deriving (ApplicativeL) via (Base1 (ST.ST s))
  deriving (MonadL) via (Base1 (ST.ST s))

type role ST nominal representational

runST :: (forall s. ST s a) -> a
runST (ST f) = case runRW# f of (# _, a #) -> a
{-# inline runST #-}
