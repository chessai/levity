module Levity.Class.Functor
  ( FunctorL(..)
  ) where

import Control.Exception (Handler)
import Control.Monad.ST (ST)
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down)
import Data.Proxy (Proxy)
import Foreign.Ptr (Ptr)
import GHC.Conc (STM)
import System.Console.GetOpt (ArgDescr, OptDescr, ArgOrder)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

--import GHC.Prim
--import GHC.Types

import GHC.Base (Type)
import GHC.Prim

import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified GHC.Generics as Generic
import qualified Prelude

import Levity.Types.Base

class FunctorL (f :: Type -> TYPE r) where
  {-# minimal fmapL #-}
  fmapL :: (a -> b) -> (f a -> f b)

instance Prelude.Functor f => FunctorL (Base1 f) where
  fmapL = Prelude.fmap

deriving via (Base1 []) instance FunctorL []
deriving via (Base1 Prelude.Maybe) instance FunctorL Prelude.Maybe
deriving via (Base1 Prelude.IO) instance FunctorL Prelude.IO
deriving via (Base1 Generic.Par1) instance FunctorL Generic.Par1
deriving via (Base1 NonEmpty) instance FunctorL NonEmpty
deriving via (Base1 ReadP) instance FunctorL ReadP
deriving via (Base1 ReadPrec) instance FunctorL ReadPrec
deriving via (Base1 Down) instance FunctorL Down
{-
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
deriving via (Base1 x) instance FunctorL x
-}
--deriving via (Base1 (Generic.Rec1 f)) instance FunctorL (Generic.Rec1 f)
deriving via (Base1 (Semigroup.Arg a)) instance FunctorL (Semigroup.Arg a)
--deriving via (Base1 Proxy) instance FunctorL Proxy
deriving via (Base1 (ST s)) instance FunctorL (ST s)
deriving via (Base1 ((,) a)) instance FunctorL ((,) a)
--deriving via (Base1 Generic.U1) instance FunctorL Generic.U1
--deriving via (Base1 Generic.V1) instance FunctorL Generic.V1
deriving via (Base1 (Prelude.Either a)) instance FunctorL (Prelude.Either a)
deriving via (Base1 Complex) instance FunctorL Complex
deriving via (Base1 Semigroup.Min) instance FunctorL Semigroup.Min
deriving via (Base1 Semigroup.Max) instance FunctorL Semigroup.Max
deriving via (Base1 Semigroup.First) instance FunctorL Semigroup.First
deriving via (Base1 Semigroup.Last) instance FunctorL Semigroup.Last
deriving via (Base1 Semigroup.Option) instance FunctorL Semigroup.Option
deriving via (Base1 ArgOrder) instance FunctorL ArgOrder
deriving via (Base1 OptDescr) instance FunctorL OptDescr
deriving via (Base1 ArgDescr) instance FunctorL ArgDescr
--deriving via (Base1 ZipList) instance FunctorL ZipList
deriving via (Base1 Identity) instance FunctorL Identity
deriving via (Base1 Handler) instance FunctorL Handler
deriving via (Base1 STM) instance FunctorL STM
deriving via (Base1 Monoid.First) instance FunctorL Monoid.First
deriving via (Base1 Monoid.Last) instance FunctorL Monoid.Last
deriving via (Base1 Monoid.Dual) instance FunctorL Monoid.Dual
deriving via (Base1 Monoid.Sum) instance FunctorL Monoid.Sum
deriving via (Base1 Monoid.Product) instance FunctorL Monoid.Product

{-
deriveFunctor(Semigroup.Arg a)
deriveFunctorC2(Prelude.Functor f, Functor f, Generic.Rec1 f)
deriveFunctor(Generic.URec Prelude.Char)
deriveFunctor(Generic.URec Prelude.Double)
deriveFunctor(Generic.URec Prelude.Float)
deriveFunctor(Generic.URec Prelude.Int)
deriveFunctor(Generic.URec Prelude.Word)
deriveFunctor(Generic.URec (Ptr ()))
deriveFunctorC2(Prelude.Functor f, Functor f, Monoid.Alt f)
#if MIN_VERSION_base(4,12,0)
deriveFunctorC2(Prelude.Functor f, Functor f, Monoid.Ap f)
#endif
deriveFunctor(Const m)
deriveFunctor((->) r)
deriveFunctor(Generic.K1 i c)
deriveFunctorC4(Prelude.Functor f, Prelude.Functor g, Functor f, Functor g, (f Generic.:+: g))
deriveFunctorC4(Prelude.Functor f, Prelude.Functor g, Functor f, Functor g, (f Generic.:*: g))
instance Functor ((# , #) a) where
  fmap f (# a, b #) = (# a, f b #)

instance Functor ((# ,, #) a b) where
  fmap f (# a, b, c #) = (# a, b, f c #)

#define deriveFunctor(ty)     \
instance Functor (ty) where { \
    fmap = Prelude.fmap       \
}

#define deriveFunctorC2(c1,c2,ty)         \
instance (c1, c2) => Functor (ty) where { \
    fmap = Prelude.fmap                   \
}

#define deriveFunctorC4(c1,c2,c3,c4,ty)           \
instance (c1, c2, c3, c4) => Functor (ty) where { \
    fmap = Prelude.fmap                           \
}


-}
