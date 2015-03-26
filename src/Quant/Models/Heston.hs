{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}


module Quant.Models.Heston (
    Heston (..)
) where

import Quant.YieldCurve
import Data.Random
import Quant.Models
import Control.Monad.State
import Quant.MonteCarlo
import qualified Data.Vector.Unboxed as U

{- | 'Heston' represents a Heston model (i.e. stochastic volatility)
model with a yield curve for a 
-}
data Heston = forall a b  . (YieldCurve a, YieldCurve b) => Heston {
    hestonInit       :: Double
  , hestonV0         :: Double
  , hestonVF         :: Double
  , hestonLambda     :: Double
  , hestonCorrel     :: Double
  , hestonMeanRev    :: Double
  , hestonForwardGen :: a
  , hestonDisc       :: b }

instance Discretize Heston where
    initialize (Heston s v0 _ _ _ _ _ _) trials = put (Observables [U.replicate trials s,
                                                                    U.replicate trials v0 ], 0)

    evolve' h@(Heston _ _ vf l rho eta _ _) t2 anti = do
        (Observables (sState:vState:_), t1) <- get
        fwd <- forwardGen h t2
        let grwth = U.map (\(g, v) -> (g - v/2) * (t2-t1)) (U.zip fwd vState)
            t = t2-t1
        states <- U.forM (U.zip3 grwth sState vState) $ \ ( g , x, v ) -> do
             resid1  <- lift stdNormal
             resid2' <- lift stdNormal
             let 
                op = if anti then (-) else (+)
                resid2 = rho * resid1 + sqrt (1-rho*rho) * resid2'
                v' = (sqrt v `op` eta/2.0*sqrt t* resid2)^(2 :: Int)-l*(v-vf)*t-eta*eta*t/4.0
                s' = x * exp (g `op` resid1*sqrt v)
             return (s', abs v')
        let newS = U.map fst states
            newV = U.map snd states
        put (Observables [newS, newV], t2)

    discounter (Heston _ _ _ _ _ _ _ d) t = do
        size <- getTrials
        return $ U.replicate size $ disc d t

    forwardGen (Heston _ _ _ _ _ _ fg _) t2 = do
        size <- getTrials
        t1 <- gets snd
        return $ U.replicate size $ forward fg t1 t2

    maxStep _ = 1/250