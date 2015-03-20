{-# LANGUAGE TypeFamilies #-}

module Quant.Models (
    Discretize (..)
  , CharFunc(..)
) where

import Control.Monad.MonteCarlo
import Control.Monad.State
import Data.Complex
import Quant.RNProcess
import Quant.YieldCurve

{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize' and 'evolve'.
-}
class Discretize a where
    -- | Represents the internal state of the function.
    type InternalState

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a => a -> Int -> MonteCarlo InternalState ()

    -- | Evolves the internal states of the MC variables between two times.
    -- | First variable is t0, second is t1
    evolve :: Discretize a => a -> Double -> Double -> MonteCarlo InternalState ()

    -- | Evolves to terminal values.
    evolveTerminal :: Discretize a => a -> Double -> Int -> MonteCarlo InternalState ()
    evolveTerminal model t steps = evolveTimeSteps model (init vals) (tail vals)
        where 
          dt = t / fromIntegral steps
          vals = take (steps+1) $ iterate (+dt) 0.0
      
    -- | Evolves based on a series of of start/end vals.
    evolveTimeSteps :: Discretize a => a -> [Double] -> [Double] -> MonteCarlo InternalState ()
    evolveTimeSteps model t0 t1 = forM_ vals $ uncurry $ evolve model
      where vals = zip t0 t1
      

{- | The 'CharFunc' class defines those
models which have closed-form characteristic
functions.

Minimal complete definition: 'charFunc'.
-}
class CharFunc a where
    -- | Creates a characteristic function for a model, without martingale adjustment.
    charFunc :: CharFunc a => a -> Double -> Complex Double -> Complex Double

    -- | Calculates characteristic function given a forward generator and yield curve.
    charFuncMart :: (CharFunc a, ForwardGen b) => a -> b -> Double -> Complex Double -> Complex Double
    charFuncMart model fg t k = exp (i * r * k) * baseCF k
      where 
        i = 0 :+ 1
        baseCF = charFunc model t
        r = forwardRN fg 0 t :+ 0

    charFuncOption :: (CharFunc a, ForwardGen b, YieldCurve c) => 
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