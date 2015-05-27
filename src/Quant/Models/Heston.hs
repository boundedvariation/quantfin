{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Quant.Models.Heston (
    Heston (..)
) where

import Quant.Time
import Quant.Types
import Quant.YieldCurve
import Data.Random
import Control.Monad.State
import Quant.MonteCarlo
import Quant.VectorOps
import qualified Data.Vector.Unboxed as U

-- | 'Heston' represents a Heston model (i.e. stochastic volatility).
data Heston = forall a b  . (YieldCurve a, YieldCurve b) => Heston {
    hestonInit       :: Double  -- ^ Initial asset level.
  , hestonV0         :: Double  -- ^ Initial variance
  , hestonVF         :: Double  -- ^ Mean-reversion variance
  , hestonLambda     :: Double  -- ^ Vol-vol
  , hestonCorrel     :: Double  -- ^ Correlation between processes
  , hestonMeanRev    :: Double  -- ^ Mean reversion speed
  , hestonForwardGen :: a       -- ^ 'YieldCurve' to generate forwards
  , hestonDisc       :: b }     -- ^ 'YieldCurve' to generate discounts

instance Discretize Heston Observables2 where
    initialize (Heston s v0 _ _ _ _ _ _) = put (Observables2 
                                                  (constant s) 
                                                  (constant v0)
                                                , Time 0)
    {-# INLINE initialize #-}

    evolve' h@(Heston _ _ vf l rho eta _ _) t2 anti = do
        (Observables2 sState vState, t1) <- get
        fwd <- forwardGen h t2
        let grwth = (fwd - vState/2) .* t
            t = timeDiff t1 t2
        resid1  <- lift $ U.replicateM mcVecLen stdNormal
        resid2' <- lift $ U.replicateM mcVecLen stdNormal
        let 
          op = if anti then (-) else (+)
          resid2 = rho *. resid1 + sqrt (1-rho*rho) *. resid2'
          v' = (sqrt vState `op` (eta/2.0*sqrt t*. resid2))^(2 :: Int)-l*.(vState.-vf).*t.-eta*eta*t/4.0
          s' = sState * exp (grwth `op` (resid1*sqrt (vState.*t)))
        put (Observables2 s' (abs v'), t2)
    {-# INLINE evolve' #-}

    discount (Heston _ _ _ _ _ _ _ d) t = return $ constant $ disc d t
    {-# INLINE discount #-}

    forwardGen (Heston _ _ _ _ _ _ fg _) t2 = do
        t1 <- gets snd
        return $ constant $ forward fg t1 t2
    {-# INLINE forwardGen #-}

    maxStep _ = 1/12