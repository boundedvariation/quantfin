module Quant.Math.Interpolation (
    linearInterpolator
  , logLinearInterpolator
  , linearVarianceInterpolator
  , cSplineInterpolator
  , Interpolator1d
  --, cubicSpline
) where

import Quant.Math.Utilities (tdmaSolver)
import Data.List (zipWith5)

type Interpolator1d = [Double] -> [Double] -> Double -> Double

linearInterpolator :: Interpolator1d
linearInterpolator (x1:x2:xs) (y1:y2:ys) x
    | x >= x2   = linearInterpolator (x2:xs) (y2:ys) x
    | x <= x1   = y1
    | otherwise = wt1 * y1 + wt2 * y2
        where
          wt1 = (x2-x) / (x2-x1)
          wt2 = (x-x1) / (x2-x1)
linearInterpolator _ [y] _ = y

logLinearInterpolator :: Interpolator1d
logLinearInterpolator x1 x2 x = exp $ linearInterpolator x1 (map log x2) x

linearVarianceInterpolator :: Interpolator1d
linearVarianceInterpolator xs ys = linearInterpolator xs 
                                 . map (\(x, y) -> y*y*x) 
                                 $ zip xs ys

cSplineInterpolator :: Interpolator1d
cSplineInterpolator xs ys x = evalSpline xs ys moments
  where
    h = zipWith (-) (tail xs) (init xs)
    lambda = 0.0 : zipWith (\a b->a/(a+b)) (tail h) (init h)
    mu = map (\a->1.0-a) lambda++[0.0]
    dj hj hj1 yj1 yj yjm1= 6.0/(hj+hj1)*((yj1-yj)/hj1-(yj-yjm1)/hj)
    d = 0.0 : zipWith5 dj (init h) (tail h) 
        (tail $ tail ys) (tail $ init ys) 
        (init $ init ys) ++ [0.0] 
    moments = tdmaSolver mu (replicate (length d) 2) lambda d
    evalSpline (x1:x2:xs') (y1:y2:ys') (m1:m2:ms)
      | x <= x1   = y1
      | x >= x2   = evalSpline (x2:xs') (y2:ys') (m2:ms)
      | otherwise = y1+beta*term+gamma*term*term+
                      delta*term*term*term
          where
            gamma = m1/2.0
            beta = (y2-y1)/h'-(2*m1+m2)*h'/6.0
            delta = (m2-m1)/6.0/h'
            term = x-x1
            h' = x2-x1
    evalSpline _ (y':_) _ = y'