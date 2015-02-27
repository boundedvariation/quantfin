{-# LANGUAGE MultiParamTypeClasses #-}

module Quant.Models (
    Discretize (..)
  , CharFunc(..)
) where

import Control.Monad.State
import Data.Complex

{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize' and 'evolve'.
-}
class Discretize a k where
    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a k => a -> Int -> State k ()

    -- | Evolves the internal states of the MC variables between two times.
    evolve :: Discretize a k => a -> Double -> Double -> State k ()

{- | The 'CharFunc' class defines those
models which have analytic characteristic
functions.

Minimal complete definition: 'charFunc'.
-}
class CharFunc a where
    -- | Creates a function for a characteristic functions.
    charFunc :: CharFunc a => a -> Double -> Complex Double -> Complex Double