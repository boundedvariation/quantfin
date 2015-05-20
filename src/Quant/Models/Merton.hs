{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Merton (
    Merton (..)
) where

import Quant.Time
import Data.Random
import Data.Random.Distribution.Poisson
import Control.Monad.State
import Quant.MonteCarlo
import Quant.ContingentClaim
import Quant.YieldCurve

-- | 'Merton' represents a Merton model (Black-Scholes w/ jumps).
data Merton = forall a b . (YieldCurve a, YieldCurve b) => Merton {
   mertonInitial     ::  Double -- ^ Initial asset level
 , mertonVol         ::  Double -- ^ Asset volatility
 , mertonIntensity   ::  Double -- ^ Intensity of Poisson process
 , mertonJumpMean    ::  Double -- ^ Average size of jump
 , mertonJumpVol     ::  Double -- ^ Volatility of jumps
 , mertonForwardGen  ::  a      -- ^ 'YieldCurve' to generate forwards
 , mertonDiscounter  ::  b }    -- ^ 'YieldCurve' to generate discount rates

--instance CharFunc Merton where
  --  charFunc (Merton s vol intensity mu sig fg) t k = charFunc (Black s vol fg) t k * addon
    --    where
      --      i = 0 :+ 1
        --    inner1 = exp (mu + sig*sig/2) :+ 0
          --  inner2 = exp $ i * k * (mu :+ 0) -  k*k*(sig :+ 0) * (sig :+ 0)/2
            --addon = exp $ (intensity * t :+ 0) * (-i*k*(inner1 - 1) + inner2 - 1)

instance Discretize Merton where
    initialize (Merton s _ _ _ _ _ _) = put (Observables [s], Time 0)
    {-# INLINE initialize #-}

    evolve' m@(Merton _ vol intensity mu sig _ _) t2 anti = do
        (Observables (stateVal:_), t1) <- get
        fwd <- forwardGen m t2
        let correction = exp (mu + sig*sig /2.0) - 1
            grwth = (fwd - vol*vol/2 - intensity * correction) * t
            t = timeDiff t1 t2
        normResid1 <- lift stdNormal
        normResid2 <- lift stdNormal
        poissonResid <- lift $ integralPoisson (intensity * t) :: MonteCarlo (MCObservables, Time) Int
        let  poisson' = fromIntegral poissonResid
             jumpterm = mu*poisson'+sig*sqrt poisson' * normResid2
             s' | anti      = stateVal * exp (grwth - normResid1*vol + jumpterm)
                | otherwise = stateVal * exp (grwth + normResid1*vol + jumpterm)
        put (Observables [s'], t2)
    {-# INLINE evolve' #-}

    discount (Merton _ _ _ _ _ _ dsc) t = disc dsc t
    {-# INLINE discount #-}

    forwardGen (Merton _ _ _ _ _ fg _) t2 = do
        t1 <- gets snd
        return $ forward fg t1 t2
    {-# INLINE forwardGen #-}