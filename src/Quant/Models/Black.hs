{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Quant.Models.Black (
    Black (..)
) where

import Quant.YieldCurve
import Data.Random
import Quant.Models
import Control.Applicative
import Control.Monad.State
import qualified Data.Vector.Unboxed as U

{- | 'Black' represents a Black-Scholes
model with a yield curve for a 
-}
data Black = forall a b  . (YieldCurve a, YieldCurve b) => Black {
    blackInit       :: Double
  , blackVol        :: Double
  , blackForwardGen :: a
  , blackYieldCurve :: b }

--instance CharFunc Black where
  --  charFunc (Black s vol _ _) t k = exp 
    --    $ i*logs + negate i*vol'*vol'/2.0*t'*k-vol'*vol'*k*k/2.0*t'
      --  where
        --    i = 0 :+ 1
          --  t' = t :+ 0
            --vol' = vol :+ 0
            --logs = log s :+ 0

instance Discretize Black (U.Vector Double) where
    initialize (Black s _ _ _) trials = put (U.replicate trials s, 0)

    evolve' b@(Black _ vol _ _) t2 = do
        (stateVec, t1) <- get
        fwd <- forwardGen b t2
        let grwth = U.map (\x -> (x - vol*vol/2) * (t2-t1)) fwd
        postVal <- U.forM (U.zip grwth stateVec) $ \ ( g , x ) -> do
             resid <- lift stdNormal
             return $ x * exp (g + resid*vol)
        put (postVal, t2)

    discounter (Black _ _ _ dsc) t = do
        size <- U.length <$> gets fst
        return $ U.replicate size $ disc dsc t

    forwardGen (Black _ _ fg _) t2 = do
        size <- U.length <$> gets fst
        t1 <- gets snd
        return $ U.replicate size $ forward fg t1 t2

    minStep _ _ = 100