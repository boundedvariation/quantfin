{-# LANGUAGE TypeFamilies #-}

module Quant.Models (
    Discretize (..)
  , CharFunc(..)
) where

import Control.Monad.MonteCarlo
import Data.Complex

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
    --evolveTerminal :: Discretize a => Double -> Int -> MonteCarlo a ()
    --evolveTerminal 

{- | The 'CharFunc' class defines those
models which have closed-form characteristic
functions.

Minimal complete definition: 'charFunc'.
-}
class CharFunc a where
    -- | Creates a function for a characteristic functions.
    charFunc :: CharFunc a => a -> Double -> Complex Double -> Complex Double

