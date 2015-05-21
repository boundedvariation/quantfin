{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Dupire (
    Dupire (..)
) where

import Quant.Time
import Data.Random
import Control.Monad.State
import Quant.ContingentClaim
import Quant.MonteCarlo
import Quant.YieldCurve

-- | 'Dupire' represents a Dupire-style local vol model.
data Dupire = forall a b . (YieldCurve a, YieldCurve b) => Dupire {
   dupireInitial     ::  Double -- ^ Initial asset level
 , dupireFunc        ::  Time -> Double -> Double -- ^ Local vol function taking a time to maturity and a level
 , mertonForwardGen  ::  a  -- ^ 'YieldCurve' to generate forwards
 , mertonDiscounter  ::  b } -- ^ 'YieldCurve' to generate discount rates

instance Discretize Dupire where
    initialize (Dupire s _ _ _) = put (Observables [s], Time 0)
    {-# INLINE initialize #-}

    evolve' d@(Dupire _ f _ _) t2 anti = do
        (Observables (stateVal:_), t1) <- get
        fwd <- forwardGen d t2
        let vol   = f t1 stateVal
            grwth = (fwd - vol * vol / 2) * timeDiff t1 t2
        normResid <- lift stdNormal
        let s' | anti      = stateVal * exp (grwth - normResid*vol)
               | otherwise = stateVal * exp (grwth - normResid*vol)
        put (Observables [s'], t2)
    {-# INLINE evolve' #-}

    discount (Dupire _ _ _ dsc) t = return $ disc dsc t
    {-# INLINE discount #-}

    forwardGen (Dupire _ _ fg _) t2 = do
        t1 <- gets snd
        return $ forward fg t1 t2
    {-# INLINE forwardGen #-}