{-# LANGUAGE ExistentialQuantification #-}


module Quant.Models.Black (
    Black (..)
) where

import Quant.Time
import Quant.YieldCurve
import Data.Random
import Control.Monad.State
import Quant.MonteCarlo
import Quant.ContingentClaim

-- | 'Black' represents a Black-Scholes model.
data Black = forall a b  . (YieldCurve a, YieldCurve b) => Black {
    blackInit       :: Double -- ^ Initial asset level.
  , blackVol        :: Double -- ^ Volatility.
  , blackForwardGen :: a      -- ^ 'YieldCurve' to generate forwards
  , blackYieldCurve :: b }    -- ^ 'YieldCurve' to handle discounting

--instance CharFunc Black where
  --  charFunc (Black s vol _ _) t k = exp 
    --    $ i*logs + negate i*vol'*vol'/2.0*t'*k-vol'*vol'*k*k/2.0*t'
      --  where
        --    i = 0 :+ 1
          --  t' = t :+ 0
            --vol' = vol :+ 0
            --logs = log s :+ 0

instance Discretize Black where
    initialize (Black s _ _ _)  = put (Observables [s], Time 0)
    {-# INLINE initialize #-}

    evolve' b@(Black _ vol _ _) t2 anti = do
        (Observables (stateVal:_), t1) <- get
        fwd <- forwardGen b t2
        let  grwth = (fwd - vol*vol/2) * timeDiff t1 t2
        postVal <- do
             resid <- lift stdNormal
             if anti then
                return $ stateVal * exp (grwth - resid*vol)
             else
                return $ stateVal * exp (grwth + resid*vol)
        put (Observables [postVal], t2)
    {-# INLINE evolve' #-}

    discount (Black _ _ _ dsc) t = disc dsc t
    {-# INLINE discount #-}

    forwardGen (Black _ _ fg _) t2 = do
      (_, t1) <- get
      return $ forward fg t1 t2
    {-# INLINE forwardGen #-}

    maxStep _ = 100