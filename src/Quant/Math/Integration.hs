module Quant.Math.Integration (
    Integrator
  , midpoint
  , trapezoid
  , simpson
    ) where

-- | A function, a lower bound, an upper bound and returns the integrated value.
type Integrator = (Double -> Double) -> Double -> Double -> Double

-- | Midpoint integration.
midpoint :: Int -> Integrator
midpoint intervals f lBound uBound = dx * sum (map f points)
    where
        dx = (uBound - lBound) / fromIntegral intervals
        points = take intervals $ iterate (+dx) (lBound+dx/2)

-- | Trapezoidal integration.
trapezoid :: Int -> Integrator
trapezoid intervals f lBound uBound = dx * ((f lBound + f uBound) / 2 + sum (map f points))
    where
        dx = (uBound - lBound) / fromIntegral intervals
        points = take (intervals-1) $ iterate (+dx) (lBound+dx)

-- | Integration using Simpson's rule.
simpson :: Int -> Integrator
simpson intervals f l u =( 2 * midpoint intervals f l u  + trapezoid intervals f l u ) / 3