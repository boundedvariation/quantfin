{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Quant.Models.Black (
    Black (..)
) where

import Quant.RNProcess
import Quant.Models
import Data.Complex
import Control.Monad.State
import qualified Data.Vector.Unboxed as U

{- | 'Black' represents a Black-Scholes
model with a yield curve for a 
-}
data Black = forall a . ForwardGen a => Black !Double !Double !a

data BlackState = BlackState (U.Vector Double)

instance CharFunc Black where
    charFunc (Black _ vol _) t k = exp 
        $ negate i*vol'*vol'/2.0*t'*k-vol'*vol'*k*k/2.0*t'
        where
            i = 0 :+ 1
            t' = t :+ 0
            vol' = vol :+ 0

instance Discretize Black BlackState where
    initialize (Black s _ _) trials = put 
                $ BlackState 
                $ U.replicate trials s
    evolve = undefined