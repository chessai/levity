module Levity.Class.Functor
  ( FunctorL(..)
  ) where

import Control.Applicative (ZipList)
import Control.Exception (Handler)
import Control.Monad.ST (ST)
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid qualified as Monoid
import Data.Ord (Down)
import Data.Proxy (Proxy)
import Data.Semigroup qualified as Semigroup
import Foreign.Ptr (Ptr)
import GHC.Base (Type)
import GHC.Conc (STM)
import GHC.Generics qualified as Generic
import GHC.Prim
import Prelude
import System.Console.GetOpt (ArgDescr, OptDescr, ArgOrder)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

import Levity.Types.Base

class FunctorL (f :: Type -> TYPE r) where
  {-# minimal fmapL #-}
  fmapL :: (a -> b) -> (f a -> f b)

instance Prelude.Functor f => FunctorL (Base1 f) where
  fmapL = Prelude.fmap

-- Manually-defined instances are because of: https://gitlab.haskell.org/ghc/ghc/-/issues/20680
deriving via (Base1 []) instance FunctorL []
deriving via (Base1 Prelude.Maybe) instance FunctorL Prelude.Maybe
deriving via (Base1 Prelude.IO) instance FunctorL Prelude.IO
deriving via (Base1 Generic.Par1) instance FunctorL Generic.Par1
deriving via (Base1 NonEmpty) instance FunctorL NonEmpty
deriving via (Base1 ReadP) instance FunctorL ReadP
deriving via (Base1 ReadPrec) instance FunctorL ReadPrec
deriving via (Base1 Down) instance FunctorL Down
deriving via (Base1 (Semigroup.Arg a)) instance FunctorL (Semigroup.Arg a)
deriving via (Base1 (ST s)) instance FunctorL (ST s)
deriving via (Base1 ((,) a)) instance FunctorL ((,) a)
deriving via (Base1 (Prelude.Either a)) instance FunctorL (Prelude.Either a)
deriving via (Base1 Complex) instance FunctorL Complex
deriving via (Base1 Semigroup.Min) instance FunctorL Semigroup.Min
deriving via (Base1 Semigroup.Max) instance FunctorL Semigroup.Max
deriving via (Base1 Semigroup.First) instance FunctorL Semigroup.First
deriving via (Base1 Semigroup.Last) instance FunctorL Semigroup.Last
deriving via (Base1 ArgOrder) instance FunctorL ArgOrder
deriving via (Base1 OptDescr) instance FunctorL OptDescr
deriving via (Base1 ArgDescr) instance FunctorL ArgDescr
deriving via (Base1 Identity) instance FunctorL Identity
deriving via (Base1 Handler) instance FunctorL Handler
deriving via (Base1 STM) instance FunctorL STM
deriving via (Base1 Monoid.First) instance FunctorL Monoid.First
deriving via (Base1 Monoid.Last) instance FunctorL Monoid.Last
deriving via (Base1 Monoid.Dual) instance FunctorL Monoid.Dual
deriving via (Base1 Monoid.Sum) instance FunctorL Monoid.Sum
deriving via (Base1 Monoid.Product) instance FunctorL Monoid.Product
deriving via (Base1 ZipList) instance FunctorL ZipList

deriving via (Base1 Generic.U1) instance FunctorL Generic.U1
deriving via (Base1 Generic.V1) instance FunctorL Generic.V1
instance (Prelude.Functor f) => FunctorL (Generic.Rec1 f) where { fmapL = Prelude.fmap }
deriving via (Base1 (Generic.URec (Ptr ()))) instance FunctorL (Generic.URec (Ptr ()))
deriving via (Base1 (Generic.URec Char)) instance FunctorL (Generic.URec Char)
deriving via (Base1 (Generic.URec Double)) instance FunctorL (Generic.URec Double)
deriving via (Base1 (Generic.URec Float)) instance FunctorL (Generic.URec Float)
deriving via (Base1 (Generic.URec Int)) instance FunctorL (Generic.URec Int)
deriving via (Base1 (Generic.K1 i c)) instance FunctorL (Generic.K1 i c)
deriving via (Base1 Proxy) instance FunctorL Proxy
deriving via (Base1 (Const m)) instance FunctorL (Const m)
instance (Functor f) => FunctorL (Monoid.Alt f) where { fmapL = fmap }
instance (Functor f) => FunctorL (Monoid.Ap f) where { fmapL = fmap }
deriving via (Base1 ((->) r)) instance FunctorL ((->) r)
instance (Functor f, Functor g) => FunctorL (f Generic.:+: g) where { fmapL = fmap }
instance (Functor f, Functor g) => FunctorL (f Generic.:*: g) where { fmapL = fmap }
instance FunctorL ((# , #) a) where { fmapL f (# a, b #) = (# a, f b #) }
instance FunctorL ((# ,, #) a b) where { fmapL f (# a, b, c #) = (# a, b, f c #) }
