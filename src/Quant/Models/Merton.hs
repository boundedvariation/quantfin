{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Merton (
    Merton (..)
) where

import Data.Random
import Data.Random.Distribution.Poisson
import Control.Applicative
import Control.Monad.State
import Quant.MonteCarlo
import Quant.YieldCurve
import qualified Data.Vector.Unboxed as U

{- | 'Merton' represents a Merton model (Black-Scholes w/ jumps).

-}
data Merton = forall a b . (YieldCurve a, YieldCurve b) => Merton {
   mertonInitial     ::  Double
 , mertonVol         ::  Double
 , mertonIntensity   ::  Double
 , mertonJumpMean    ::  Double
 , mertonJumpVol     ::  Double 
 , mertonForwardGen  ::  a 
 , mertonDiscounter  ::  b } 

--instance CharFunc Merton where
  --  charFunc (Merton s vol intensity mu sig fg) t k = charFunc (Black s vol fg) t k * addon
    --    where
      --      i = 0 :+ 1
        --    inner1 = exp (mu + sig*sig/2) :+ 0
          --  inner2 = exp $ i * k * (mu :+ 0) -  k*k*(sig :+ 0) * (sig :+ 0)/2
            --addon = exp $ (intensity * t :+ 0) * (-i*k*(inner1 - 1) + inner2 - 1)

instance Discretize Merton where
    initialize (Merton s _ _ _ _ _ _) trials = put (Observables [U.replicate trials s], 0)

    evolve' m@(Merton _ vol intensity mu sig _ _) t2 = do
        (Observables (stateVec:_), t1) <- get
        fwd <- forwardGen m t2
        let correction = exp (mu + sig*sig /2.0) - 1
            grwth = U.map (\x -> (x - vol*vol/2 - intensity * correction) * (t2-t1)) fwd
        postVal <- U.forM (U.zip grwth stateVec) $ \ ( g,x ) -> do
             normResid1 <- lift stdNormal
             normResid2 <- lift stdNormal
             poissonResid <- lift $ integralPoisson (intensity * (t2-t1)) :: MonteCarlo (Observables, Double) Int
             let  poisson' = fromIntegral poissonResid
                  jumpterm = mu*poisson'+sig*sqrt poisson' * normResid2
             return $ x * exp (g + normResid1*vol + jumpterm)
        put (Observables [postVal], t2)

    discounter (Merton _ _ _ _ _ _ dsc) t = do
        size <- U.length <$> gets (obsPull . fst)
        return $ U.replicate size $ disc dsc t

    forwardGen (Merton _ _ _ _ _ fg _) t2 = do
        size <- U.length <$> gets (obsPull . fst)
        t1 <- gets snd
        return $ U.replicate size $ forward fg t1 t2