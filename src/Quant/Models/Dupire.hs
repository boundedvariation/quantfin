{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Dupire (
    Dupire (..)
) where

import Data.Random
import Control.Applicative
import Control.Monad.State
import Quant.MonteCarlo
import Quant.YieldCurve
import qualified Data.Vector.Unboxed as U

{- | 'Dupire' represents a Dupire-style local vol model.

-}
data Dupire = forall a b . (YieldCurve a, YieldCurve b) => Dupire {
   dupireInitial     ::  Double
 , dupireFunc        ::  Double -> Double -> Double
 , mertonForwardGen  ::  a 
 , mertonDiscounter  ::  b } 

--mkDupire s vs fg dsc = 

instance Discretize Dupire (U.Vector Double) where
    initialize (Dupire s _ _ _) trials = put (U.replicate trials s, 0)

    evolve d@(Dupire _ f _ _) t2 = do
        (stateVec, t1) <- get
        fwd <- forwardGen d t2
        let vols   = U.map (f t1) stateVec
            grwth = U.map (\(fwdVal, v) -> (fwdVal - v * v / 2) / (t2-t1)) $ U.zip fwd vols
        postVal <- U.forM (U.zip3 grwth stateVec vols) $ \ ( g,x,v ) -> do
             normResid <- lift stdNormal
             return $ x * exp (g + normResid*v)
        put (postVal, t2)

    discounter (Dupire _ _ _ dsc) t = do
        size <- U.length <$> gets fst
        return $ U.replicate size $ disc dsc t

    forwardGen (Dupire _ _ fg _) t2 = do
        (_ , t1) <- get
        size <- U.length <$> gets fst
        return $ U.replicate size $ forward fg t1 t2