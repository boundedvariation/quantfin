module Quant.Models (
    CharFunc(..)
) where

import Data.Complex
import Quant.YieldCurve


{- | The 'CharFunc' class defines those
models which have closed-form characteristic
functions.

Minimal complete definition: 'charFunc'.

Still under construction.
-}
class CharFunc a where
    -- | Creates a characteristic function for a model, without martingale adjustment.
    charFunc :: CharFunc a => a -> Double -> Complex Double -> Complex Double

    -- | Calculates characteristic function given a forward generator and yield curve.
    charFuncMart :: (CharFunc a, YieldCurve b) => a -> b -> Double -> Complex Double -> Complex Double
    charFuncMart model fg t k = exp (i * r * k) * baseCF k
      where 
        i = 0 :+ 1
        baseCF = charFunc model t
        r = forward fg 0 t :+ 0

    charFuncOption :: (CharFunc a, YieldCurve b, YieldCurve c) => 
        a -> b -> c -> ( (Double -> Double) -> Double) -> Double 
        -> Double -> Double -> Double
    charFuncOption model fg yc intF strike tmat damp = intF f
      where
        f v' = realPart $ exp (i*v*k) * leftTerm * rightTerm
          where
            v = v' :+ 0
            damp' = damp :+ 0
            k = log strike :+ 0
            i = 0 :+ 1
            leftTerm = d / (damp' + i * v) / (damp'+i*v+(1:+0))
            rightTerm = cf $ v - i * (damp' + 1)
            d = disc yc tmat :+ 0
            cf x = charFuncMart model fg tmat x