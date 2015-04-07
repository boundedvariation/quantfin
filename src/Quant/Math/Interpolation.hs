module Quant.Math.Interpolation (
    linearInterpolator
  , Interpolator1d
  , Interpolator2d
  --, cubicSpline
) where

type Interpolator1d = [Double] -> [Double] -> Double -> Double
type Interpolator2d = [Double] -> [Double] -> [[Double]] -> Double -> Double -> Double

linearInterpolator :: Interpolator1d
linearInterpolator (x1:x2:xs) (y1:y2:ys) x
    | x >= x2   = linearInterpolator (x2:xs) (y2:ys) x
    | x <= x1   = y1
    | otherwise = wt1 * y1 + wt2 * y2
        where
          wt1 = (x2-x) / (x2-x1)
          wt2 = (x-x1) / (x2-x1)