{-# language
    BangPatterns
  , RankNTypes
  , RoleAnnotations
  , UnboxedTuples
  #-}

module Levity.Types.ST
  ( ST(..)
  , runST
  , bindUnliftedToLifted
  ) where

import GHC.Base (runRW#, Type)
import GHC.Prim

import qualified Prelude
import qualified GHC.ST as ST

import Levity.Types.Base
import Levity.Class.Functor

newtype ST s (a :: TYPE r) = ST (State# s -> (# State# s, a #))
  deriving (Prelude.Functor) via (ST.ST s)
  deriving (Prelude.Applicative) via (ST.ST s)
  deriving (Prelude.Monad) via (ST.ST s)
  deriving (FunctorL) via (Base1 (ST.ST s))
  -- ,ApplicativeL,MonadL) via (Base1 (ST.ST s))

type role ST nominal representational

bindUnliftedToLifted :: ST s a -> (a -> ST s (b :: Type)) -> ST s b
bindUnliftedToLifted (ST f) g = ST
  (\s0# -> case f s0# of
    (# s1#, a #) -> case g a of
      ST h -> h s1#
  )

runST :: (forall s. ST s a) -> a
runST (ST f) = case runRW# f of (# _, a #) -> a
{-# inline runST #-}
