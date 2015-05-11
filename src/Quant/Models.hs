module Quant.Models (
    CharFunc(..)
) where

import Data.Complex
import Quant.Math.Integration
import Quant.ContingentClaim(OptionType(..))


{- | The 'CharFunc' class defines those
models which have closed-form characteristic
functions.

Minimal complete definition: 'charFunc'.

Still under construction.
-}
class CharFunc a where
    -- | Creates a characteristic function for a model.
    charFunc :: CharFunc a => a -> Double -> Complex Double -> Complex Double


    charFuncOption :: (CharFunc a) => a -> OptionType -> Integrator -> Double 
        -> Double -> Double -> Double
    charFuncOption model opt intF strike tmat damp = case opt of
        Put  -> s * q * p1 - k * disc * p2
        Call -> k * disc * (1-p2) - s * q * (1-p1)
            where
                s = 
                p1 = 0.5 + intF func1 1E-8 100 / pi
                p2 = 0.5 + intF func2 1E-8 100 / pi

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