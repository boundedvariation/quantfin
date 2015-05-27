
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
  , (.$.)
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

infixr 0 .$.
(.$.) :: U.Unbox a => (a -> a) -> U.Vector a -> U.Vector a
f .$. x = U.map f x


instance (Num a, U.Unbox a) => Num (U.Vector a) where
  (+) = (.+.)
  (-) = (.-.)
  (*) = (.*.)
  abs = U.map abs
  signum = U.map signum
  fromInteger = undefined

instance (Fractional a, U.Unbox a) => Fractional (U.Vector a) where
  (/) = (./.)
  fromRational = undefined

instance (Floating a, U.Unbox a) => Floating (U.Vector a) where
  pi = undefined
  exp = U.map exp
  log = U.map log
  sin = U.map sin
  cos = U.map cos
  tan = U.map tan
  asin = U.map asin
  acos = U.map acos
  atan = U.map atan
  sinh = U.map sinh
  cosh = U.map cosh
  tanh = U.map tanh
  asinh = U.map asinh
  acosh = U.map acosh
  atanh = U.map atanh
