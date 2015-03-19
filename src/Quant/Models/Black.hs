{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Black (
    Black (..)
) where

import Quant.RNProcess
import Data.Random
import Quant.Models
import Data.Complex
import Control.Monad.State
import qualified Data.Vector.Unboxed as U

{- | 'Black' represents a Black-Scholes
model with a yield curve for a 
-}
data Black = forall a . ForwardGen a => Black !Double !Double !a

type BlackState = U.Vector Double

instance CharFunc Black where
    charFunc (Black _ vol _) t k = exp 
        $ negate i*vol'*vol'/2.0*t'*k-vol'*vol'*k*k/2.0*t'
        where
            i = 0 :+ 1
            t' = t :+ 0
            vol' = vol :+ 0

instance Discretize Black where
    type InternalState = BlackState

    initialize (Black s _ _) trials = put 
                $ U.replicate trials s

    evolve (Black _ vol fg) t1 t2 = do
        stateVec <- get
        let fwd = forwardRN fg t1 t2
            g = (fwd - vol*vol/2) * (t2-t1)
        postVal <- U.forM stateVec $ \x -> do
             resid <- lift stdNormal
             return $ x * exp (g + resid*vol)
        put postVal