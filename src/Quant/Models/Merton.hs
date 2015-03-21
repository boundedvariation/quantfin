{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Merton (
    Merton (..)
) where

import Quant.RNProcess
import Data.Random
import Data.Random.Distribution.Poisson
import Quant.Models
import Data.Complex
import Control.Monad.State
import Control.Monad.MonteCarlo
import Quant.Models.Black
import qualified Data.Vector.Unboxed as U

{- | 'Merton' represents a Merton model (Black-Scholes w/ jumps).

-}
data Merton = forall a . ForwardGen a => Merton {
   mertonInitial     :: !Double
 , mertonVol         :: !Double
 , mertonIntensity   :: !Double
 , mertonJumpMean    ::  !Double
 , mertonJumpVol     ::  !Double 
 , mertonForwardGen  ::  !a } 

type MertonState = U.Vector Double

instance CharFunc Merton where
    charFunc (Merton s vol intensity mu sig fg) t k = charFunc (Black s vol fg) t k * addon
        where
            i = 0 :+ 1
            inner1 = exp (mu + sig*sig/2) :+ 0
            inner2 = exp $ i * k * (mu :+ 0) -  k*k*(sig :+ 0) * (sig :+ 0)/2
            addon = exp $ (intensity * t :+ 0) * (-i*k*(inner1 - 1) + inner2 - 1)

instance Discretize Merton where
    type InternalState = MertonState

    initialize (Merton s _ _ _ _ _) trials = put 
                $ U.replicate trials s

    evolve (Merton _ vol intensity mu sig fg) t1 t2 = do
        stateVec <- get
        let fwd = forwardRN fg t1 t2
            correction = exp (mu + sig*sig /2.0) - 1
            g = (fwd - vol*vol/2 - intensity * correction) * (t2-t1)
        postVal <- U.forM stateVec $ \x -> do
             normResid1 <- lift stdNormal
             normResid2 <- lift stdNormal
             poissonResid <- lift $ integralPoisson (intensity * (t2-t1)) :: MonteCarlo MertonState Int
             let  poisson' = fromIntegral poissonResid
                  jumpterm = mu*poisson'+sig*sqrt poisson' * normResid2
             return $ x * exp (g + normResid1*vol + jumpterm)
        put postVal