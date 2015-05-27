
module Quant.VectorOps (
    (.*.)
  , (.*)
  , (*.)
  , (./.)
  , (/.)
  , (./)
  , (.+.)
  , (.+)
  , (+.)
  , (.-.)
  , (.-)
  , (-.)
  ) where

import qualified Data.Vector.Unboxed as U

infixl 7 .*.
(.*.) :: (U.Unbox a, Num a) => U.Vector a -> U.Vector a -> U.Vector a
x .*. y = U.zipWith (*) x y
{-# INLINE (.*.) #-}

infixl 7 *.
(*.) :: (U.Unbox a, Num a) => a -> U.Vector a -> U.Vector a
x *. y = U.map (x*) y
{-# INLINE (*.) #-}

infixl 7 .*
(.*) :: (U.Unbox a, Num a) => U.Vector a -> a -> U.Vector a
x .* y = U.map (*y) x
{-# INLINE (.*) #-}

infixl 7 ./.
(./.) :: (U.Unbox a, Fractional a) => U.Vector a -> U.Vector a -> U.Vector a
x ./. y = U.zipWith (/) x y
{-# INLINE (./.) #-}

infixl 7 /.
(/.) :: (U.Unbox a, Fractional a) => a -> U.Vector a -> U.Vector a
x /. y = U.map (x/) y
{-# INLINE (/.) #-}

infixl 7 ./
(./) :: (U.Unbox a, Fractional a) => U.Vector a -> a -> U.Vector a
x ./ y = U.map (/y) x
{-# INLINE (./) #-}

infixl 6 .+.
(.+.) :: (U.Unbox a, Num a) => U.Vector a -> U.Vector a -> U.Vector a
x .+. y = U.zipWith (+) x y
{-# INLINE (.+.) #-}

infixl 6 +.
(+.) :: (U.Unbox a, Num a) => a -> U.Vector a -> U.Vector a
x +. y = U.map (x+) y
{-# INLINE (+.) #-}

infixl 6 .+
(.+) :: (U.Unbox a, Num a) => U.Vector a -> a -> U.Vector a
x .+ y = U.map (+y) x
{-# INLINE (.+) #-}

infixl 6 .-.
(.-.) :: (U.Unbox a, Num a) => U.Vector a -> U.Vector a -> U.Vector a
x .-. y = U.zipWith (-) x y
{-# INLINE (.-.) #-}

infixl 6 -.
(-.) :: (U.Unbox a, Num a) => a -> U.Vector a -> U.Vector a
x -. y = U.map (x-) y
{-# INLINE (-.) #-}

infixl 6 .-
(.-) :: (U.Unbox a, Num a) => U.Vector a -> a -> U.Vector a
x .- y = U.map (+negate y) x
{-# INLINE (.-) #-}