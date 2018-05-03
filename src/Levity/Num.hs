{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Levity.Num
  ( Num(..)
  ) where

import GHC.Base
import GHC.Integer
import GHC.Prim
import Prelude ()

import qualified Prelude

class Num (a :: TYPE (k :: RuntimeRep)) where
    {-# MINIMAL (+), (*), abs, signum, fromInteger, negate , (-) #-}

    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a

instance Prelude.Num a => Num (a :: Type) where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  (*) = (Prelude.*)
  negate = Prelude.negate
  abs = Prelude.abs
  signum = Prelude.signum
  fromInteger = Prelude.fromInteger

ltInt# :: Int# -> Int# -> Bool
ltInt# x y = isTrue# (x <# y)

eqInt# :: Int# -> Int# -> Bool
eqInt# x y = isTrue# (x ==# y)

instance Num Float# where
  (+) = plusFloat#
  (*) = timesFloat#
  (-) = minusFloat#
  negate = negateFloat#
  abs f# = if isTrue# (f# `gtFloat#` 0.0#) then f# else negate f#
  signum f# | isTrue# (gtFloat# f# 1.0#) = 1.0#
            | isTrue# (ltFloat# f# 1.0#) = negateFloat# 1.0#
            | otherwise = f#
  fromInteger (fromInteger -> F# f#) = f#

instance Num Int# where
  (+) = (+#)
  (*) = (*#)
  (-) = (-#)
  negate = negateInt#
  abs i# = if isTrue# (i# >=# 0#) then i# else negate i#
  signum i# | i# `ltInt#` 0# = negateInt# 1#
            | i# `eqInt#` 0# = 0#
            | otherwise     = 1#
  fromInteger (fromInteger -> I# i#) = i#

instance Num Word# where
  (+) = plusWord#
  (*) = timesWord#
  (-) = minusWord#
  negate x## = int2Word# (negateInt# (word2Int# x##))
  abs i## = if isTrue# (i## `gtWord#` 0##) then i## else negate i##
  signum 0## = 0##
  signum _   = 1##
  fromInteger (fromInteger -> W# w##) = w##

gtDouble# :: Double# -> Double# -> Bool
gtDouble# x y = isTrue# (x >## y)

ltDouble# :: Double# -> Double# -> Bool
ltDouble# x y = isTrue# (x <## y)

instance Num Double# where
  (+) = (+##)
  (*) = (*##)
  (-) = (-##)

  negate = negateDouble#
  abs    = fabsDouble#
  signum d## | gtDouble# d## 1.0## = 1.0##
             | ltDouble# d## 1.0## = negateDouble# 1.0##
             | otherwise = d##

  fromInteger (fromInteger -> D# d##) = d##

