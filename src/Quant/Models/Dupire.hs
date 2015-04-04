{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Dupire (
    Dupire (..)
) where

import Data.Random
import Control.Monad.State
import Quant.ContingentClaim
import Quant.MonteCarlo
import Quant.YieldCurve
import qualified Data.Vector.Unboxed as U

-- | 'Dupire' represents a Dupire-style local vol model.
data Dupire = forall a b . (YieldCurve a, YieldCurve b) => Dupire {
   dupireInitial     ::  Double -- ^ Initial asset level
 , dupireFunc        ::  Double -> Double -> Double -- ^ Local vol function taking a time to maturity and a level
 , mertonForwardGen  ::  a  -- ^ 'YieldCurve' to generate forwards
 , mertonDiscounter  ::  b } -- ^ 'YieldCurve' to generate discount rates

instance Discretize Dupire where
    initialize (Dupire s _ _ _) trials = put (Observables [U.replicate trials s], 0)

    evolve' d@(Dupire _ f _ _) t2 anti = do
        (Observables (stateVec:_), t1) <- get
        fwd <- forwardGen d t2
        let vols   = U.map (f t1) stateVec
            grwth = U.map (\(fwdVal, v) -> (fwdVal - v * v / 2) / (t2-t1)) $ U.zip fwd vols
        postVal <- U.forM (U.zip3 grwth stateVec vols) $ \ ( g,x,v ) -> do
             normResid <- lift stdNormal
             if anti then
                 return $ x * exp (g - normResid*v)
             else
                 return $ x * exp (g + normResid*v)
        put (Observables [postVal], t2)

    discounter (Dupire _ _ _ dsc) t = return $ disc dsc t

    forwardGen (Dupire _ _ fg _) t2 = do
        t1 <- gets snd
        return $ forward fg t1 t2