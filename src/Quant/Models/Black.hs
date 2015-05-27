{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Quant.Models.Black (
    Black (..)
) where

import Quant.Types
import Quant.Time
import Quant.YieldCurve
import Data.Random
import Control.Monad.State
import Quant.MonteCarlo

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

instance Discretize Black Observables1 where
    initialize (Black s _ _ _)  = put (Observables1 s, Time 0)
    {-# INLINE initialize #-}

    evolve' b@(Black _ vol _ _) t2 anti = do
        (Observables1 stateVal, t1) <- get
        fwd <- forwardGen b t2
        let t = timeDiff t1 t2 
            grwth = (fwd - vol*vol/2) * t
        resid <- lift stdNormal
        let resid' = if anti then -resid else resid
            postVal = stateVal * exp (grwth + resid'*vol*sqrt t)
        put (Observables1 postVal, t2)
    {-# INLINE evolve' #-}

    discount (Black _ _ _ dsc) t = return $ disc dsc t
    {-# INLINE discount #-}

    forwardGen (Black _ _ fg _) t2 = do
      (_, t1) <- get
      return $ forward fg t1 t2
    {-# INLINE forwardGen #-}

    maxStep _ = 100