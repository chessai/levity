module Levity.Class.Num
  ( Neg(..)
  , Signed(..)
  , Add(..), (+)
  , Sub(..), (-)
  , Mul(..), (*)
  , FromInteger(..)
  ) where

import GHC.Base
import GHC.Integer
import GHC.Prim
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import Data.Ratio (Ratio)
import Prelude ()

import qualified Prelude

import Levity.Types.Base

-- | Class for unary negation.
type Neg :: TYPE r -> Constraint
class Neg a where
  -- | Performs unary negation.
  neg :: a -> a

-- | Class for signed numbers (i.e. numbers that can be negative).
type Signed :: TYPE r -> Constraint
class Neg a => Signed a where
  -- | Computes the absolute value.
  --
  --   For 'Float' and 'Double', @NaN@ will be returned if the number is @NaN@.
  abs :: a -> a
  -- | Returns the sign of the number.
  --
  --   For 'Float' and 'Double':
  --   * 1.0 if the number is positive, +0.0, or infinity
  --   * -1.0 if the number is negative, -0.0, or negative infinity
  --   * @NaN@ if the number is @NaN@
  --
  --   For signed integers:
  --   * 0 if the number is zero
  --   * 1 if the number is positive
  --   * -1 if the number is negative
  signum :: a -> a

-- | Class for binary addition.
type Add :: TYPE r -> Constraint
class Add a where
  -- | Performs binary addition.
  add :: a -> a -> a

-- | Class for binary subtraction.
type Sub :: TYPE r -> Constraint
class Sub a where
  -- | Performs binary subtractin.
  sub :: a -> a -> a

-- | Class for binary multiplication.
type Mul :: TYPE r -> Constraint
class Mul a where
  -- | Performs binary multiplication.
  mul :: a -> a -> a

-- | Conversion from an 'Integer'.
type FromInteger :: TYPE r -> Constraint
class FromInteger a where
  -- | Performs conversion from an 'Integer'.
  fromInteger :: Integer -> a -- should this return Maybe a?

infixl 6 +
(+) :: forall {r :: RuntimeRep} (a :: TYPE r). (Add a) => a -> a -> a
(+) = add

infixl 6 -
(-) :: forall {r :: RuntimeRep} (a :: TYPE r). (Sub a) => a -> a -> a
(-) = sub

infixl 7 *
(*) :: forall {r :: RuntimeRep} (a :: TYPE r). (Mul a) => a -> a -> a
(*) = mul

instance Prelude.Num a => Neg (Base a) where
  neg = Prelude.negate
instance Prelude.Num a => Signed (Base a) where
  abs = Prelude.abs
  signum = Prelude.signum
instance Prelude.Num a => Add (Base a) where
  add = (Prelude.+)
instance Prelude.Num a => Sub (Base a) where
  sub = (Prelude.-)
instance Prelude.Num a => Mul (Base a) where
  mul = (Prelude.*)
instance Prelude.Num a => FromInteger (Base a) where
  fromInteger = Prelude.fromInteger

deriving via (Base Int) instance Neg Int
deriving via (Base Int8) instance Neg Int8
deriving via (Base Int16) instance Neg Int16
deriving via (Base Int32) instance Neg Int32
deriving via (Base Int64) instance Neg Int64
deriving via (Base Float) instance Neg Float
deriving via (Base Double) instance Neg Double
deriving via (Base (Ratio a)) instance (Prelude.Integral a) => Neg (Ratio a)
instance Neg Int# where { neg = negateInt# }
instance Neg Int8# where { neg = negateInt8# }
instance Neg Int16# where { neg = negateInt16# }
instance Neg Int32# where { neg = negateInt32# }
instance Neg Float# where { neg = negateFloat# }
instance Neg Double# where { neg = negateDouble# }

deriving via (Base Int) instance Signed Int
deriving via (Base Int8) instance Signed Int8
deriving via (Base Int16) instance Signed Int16
deriving via (Base Int32) instance Signed Int32
deriving via (Base Int64) instance Signed Int64
deriving via (Base Float) instance Signed Float
deriving via (Base Double) instance Signed Double
deriving via (Base (Ratio a)) instance (Prelude.Integral a) => Signed (Ratio a)
instance Signed Int# where
  abs i# = if isTrue# (i# >=# 0#) then i# else neg i#
  signum i#
    | isTrue# (i# <# 0#) = negateInt# 1#
    | isTrue# (i# ==# 0#) = 0#
    | otherwise = 1#
instance Signed Int8# where
  abs i# = if isTrue# (i# `gtInt8#` (intToInt8# 0#)) then i# else neg i#
  signum i#
    | isTrue# (i# `ltInt8#` (intToInt8# 0#)) = intToInt8# (negateInt# 1#)
    | isTrue# (i# `eqInt8#` (intToInt8# 0#)) = intToInt8# 0#
    | otherwise = intToInt8# 1#
instance Signed Int16# where
  abs i# = if isTrue# (i# `gtInt16#` (intToInt16# 0#)) then i# else neg i#
  signum i#
    | isTrue# (i# `ltInt16#` (intToInt16# 0#)) = intToInt16# (negateInt# 1#)
    | isTrue# (i# `eqInt16#` (intToInt16# 0#)) = intToInt16# 0#
    | otherwise = intToInt16# 1#
instance Signed Int32# where
  abs i# = if isTrue# (i# `gtInt32#` (intToInt32# 0#)) then i# else neg i#
  signum i#
    | isTrue# (i# `ltInt32#` (intToInt32# 0#)) = intToInt32# (negateInt# 1#)
    | isTrue# (i# `eqInt32#` (intToInt32# 0#)) = intToInt32# 0#
    | otherwise = intToInt32# 1#
instance Signed Float# where
  abs f# = if isTrue# (f# `gtFloat#` 0.0#) then f# else neg f#
  signum f# | isTrue# (f# `gtFloat#` 0.0#) = 1.0#
            | isTrue# (f# `ltFloat#` 0.0#) = neg 1.0#
            | otherwise = f#
instance Signed Double# where
  abs f# = if isTrue# (f# >## 0.0##) then f# else neg f#
  signum f# | isTrue# (f# >## 0.0##) = 1.0##
            | isTrue# (f# <## 0.0##) = neg 1.0##
            | otherwise = f#

deriving via (Base Int) instance Add Int
deriving via (Base Int8) instance Add Int8
deriving via (Base Int16) instance Add Int16
deriving via (Base Int32) instance Add Int32
deriving via (Base Int64) instance Add Int64
deriving via (Base Word) instance Add Word
deriving via (Base Word8) instance Add Word8
deriving via (Base Word16) instance Add Word16
deriving via (Base Word32) instance Add Word32
deriving via (Base Word64) instance Add Word64
deriving via (Base Float) instance Add Float
deriving via (Base Double) instance Add Double
deriving via (Base (Ratio a)) instance (Prelude.Integral a) => Add (Ratio a)
instance Add Int# where { add = (+#) }
instance Add Int8# where { add = plusInt8# }
instance Add Int16# where { add = plusInt16# }
instance Add Int32# where { add = plusInt32# }
instance Add Word# where { add = plusWord# }
instance Add Word8# where { add = plusWord8# }
instance Add Word16# where { add = plusWord16# }
instance Add Word32# where { add = plusWord32# }

deriving via (Base Int) instance Sub Int
deriving via (Base Int8) instance Sub Int8
deriving via (Base Int16) instance Sub Int16
deriving via (Base Int32) instance Sub Int32
deriving via (Base Int64) instance Sub Int64
deriving via (Base Word) instance Sub Word
deriving via (Base Word8) instance Sub Word8
deriving via (Base Word16) instance Sub Word16
deriving via (Base Word32) instance Sub Word32
deriving via (Base Word64) instance Sub Word64
deriving via (Base Float) instance Sub Float
deriving via (Base Double) instance Sub Double
deriving via (Base (Ratio a)) instance (Prelude.Integral a) => Sub (Ratio a)
instance Sub Int# where { sub = (-#) }
instance Sub Int8# where { sub = subInt8# }
instance Sub Int16# where { sub = subInt16# }
instance Sub Int32# where { sub = subInt32# }
instance Sub Word# where { sub = minusWord# }
instance Sub Word8# where { sub = subWord8# }
instance Sub Word16# where { sub = subWord16# }
instance Sub Word32# where { sub = subWord32# }

deriving via (Base Int) instance Mul Int
deriving via (Base Int8) instance Mul Int8
deriving via (Base Int16) instance Mul Int16
deriving via (Base Int32) instance Mul Int32
deriving via (Base Int64) instance Mul Int64
deriving via (Base Word) instance Mul Word
deriving via (Base Word8) instance Mul Word8
deriving via (Base Word16) instance Mul Word16
deriving via (Base Word32) instance Mul Word32
deriving via (Base Word64) instance Mul Word64
deriving via (Base Float) instance Mul Float
deriving via (Base Double) instance Mul Double
deriving via (Base (Ratio a)) instance (Prelude.Integral a) => Mul (Ratio a)
instance Mul Int# where { mul = (*#) }
instance Mul Int8# where { mul = timesInt8# }
instance Mul Int16# where { mul = timesInt16# }
instance Mul Int32# where { mul = timesInt32# }
instance Mul Word# where { mul = timesWord# }
instance Mul Word8# where { mul = timesWord8# }
instance Mul Word16# where { mul = timesWord16# }
instance Mul Word32# where { mul = timesWord32# }

deriving via (Base Int) instance FromInteger Int
deriving via (Base Int8) instance FromInteger Int8
deriving via (Base Int16) instance FromInteger Int16
deriving via (Base Int32) instance FromInteger Int32
deriving via (Base Int64) instance FromInteger Int64
deriving via (Base Word) instance FromInteger Word
deriving via (Base Word8) instance FromInteger Word8
deriving via (Base Word16) instance FromInteger Word16
deriving via (Base Word32) instance FromInteger Word32
deriving via (Base Word64) instance FromInteger Word64
deriving via (Base Float) instance FromInteger Float
deriving via (Base Double) instance FromInteger Double
deriving via (Base (Ratio a)) instance (Prelude.Integral a) => FromInteger (Ratio a)
instance FromInteger Int# where
  fromInteger (fromInteger -> I# i#) = i#
instance FromInteger Int8# where
  fromInteger (fromInteger -> I8# i#) = i#
instance FromInteger Int16# where
  fromInteger (fromInteger -> I16# i#) = i#
instance FromInteger Int32# where
  fromInteger (fromInteger -> I32# i#) = i#
instance FromInteger Word# where
  fromInteger (fromInteger -> W# w##) = w##
instance FromInteger Word8# where
  fromInteger (fromInteger -> W8# w##) = w##
instance FromInteger Word16# where
  fromInteger (fromInteger -> W16# w##) = w##
instance FromInteger Word32# where
  fromInteger (fromInteger -> W32# w##) = w##
instance FromInteger Float# where
  fromInteger (fromInteger -> F# f#) = f#
instance FromInteger Double# where
  fromInteger (fromInteger -> D# d#) = d#
