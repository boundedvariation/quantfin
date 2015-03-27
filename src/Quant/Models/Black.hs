{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}


module Quant.Models.Black (
    Black (..)
) where

import Quant.YieldCurve
import Data.Random
import Control.Monad.State
import Quant.MonteCarlo
import Quant.ContingentClaim
import qualified Data.Vector.Unboxed as U

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
    initialize (Black s _ _ _) trials = put (Observables [U.replicate trials s], 0)

    evolve' b@(Black _ vol _ _) t2 anti = do
        (Observables (stateVec:_), t1) <- get
        fwd <- forwardGen b t2
        let grwth = U.map (\x -> (x - vol*vol/2) * (t2-t1)) fwd
        postVal <- U.forM (U.zip grwth stateVec) $ \ ( g , x ) -> do
             resid <- lift stdNormal
             if anti then
                return $ x * exp (g - resid*vol)
             else
                return $ x * exp (g + resid*vol)
        put (Observables [postVal], t2)

    discounter (Black _ _ _ dsc) t = do
        size <- getTrials
        return $ U.replicate size $ disc dsc t

    forwardGen (Black _ _ fg _) t2 = do
        size <- getTrials
        t1 <- gets snd
        return $ U.replicate size $ forward fg t1 t2

    maxStep _ = 100