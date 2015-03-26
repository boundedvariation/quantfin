{-# LANGUAGE FlexibleContexts #-}


module Quant.MonteCarlo (

    MonteCarlo
  , MonteCarloT
  , runMC 

  , Discretize(..)
  , OptionType(..)
  , processClaimWithMap
  , getTrials

  )
where

import Quant.ContingentClaim
import Data.Random
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.RVar
import System.Random.Mersenne.Pure64
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U

type MonteCarloT m s = StateT s (RVarT m)

type MonteCarlo s a = MonteCarloT Identity s a

runMC :: MonadRandom (StateT b Identity) => MonteCarlo s c -> b -> s -> c
runMC mc randState initState = flip evalState randState $ sampleRVarTWith lift (evalStateT mc initState)


{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize', 'discounter', 'forwardGen' and 'evolve'.
-}

class Discretize a where

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a => a -> Int -> MonteCarlo (Observables, Double) ()

    -- | Evolves the internal states of the MC variables between two times.
    -- | First variable is t0, second is t1
    evolve :: Discretize a => a -> Double -> Bool -> MonteCarlo (Observables, Double) ()
    evolve mdl t2 anti = do
        (_, t1) <- get
        let ms = maxStep mdl
        if (t2-t1) < ms then 
            evolve' mdl t2 anti
        else do
            evolve' mdl (t1 + ms) anti
            evolve mdl t2 anti

    discounter :: Discretize a => a -> Double -> MonteCarlo (Observables, Double) (U.Vector Double)

    forwardGen :: Discretize a => a -> Double -> MonteCarlo (Observables, Double) (U.Vector Double)

    evolve' :: Discretize a => a -> Double -> Bool -> MonteCarlo (Observables, Double) ()

    maxStep :: Discretize a => a -> Double --need to think of a way to get the b out with introducing a mess.
    maxStep _ = 1/250

    simulateState :: Discretize a => 
        a -> ContingentClaimBasket -> Int -> Bool ->
        MonteCarlo (Observables, Double) Double
    simulateState modl (ContingentClaimBasket cs ts) trials anti = do
        initialize modl trials
        avg <$> process Map.empty (U.replicate trials 0) cs ts
            where 
                process m cfs ccs@(c@(ContingentClaim' t _ _):cs') (obsT:ts') = 
                    if t >= obsT then do
                        evolve modl obsT anti
                        obs <- gets fst
                        let m' = Map.insert obsT obs m
                        process m' cfs ccs ts'
                    else do
                        evolve modl t anti
                        let cfs' = processClaimWithMap c m
                        d <- discounter modl obsT
                        let cfs'' = cfs' |*| d
                        process m (cfs |+| cfs'') cs' (obsT:ts')

                process m cfs (c:cs') [] = do
                    d <- discounter modl (payoutTime c)
                    let cfs' = d |*| processClaimWithMap c m
                    process m (cfs |+| cfs') cs' []

                process _ cfs _ _ = return cfs

                v1 |+| v2 = U.zipWith (+) v1 v2
                v1 |*| v2 = U.zipWith (*) v1 v2

                avg v = U.sum v / fromIntegral (U.length v)

    runSimulation :: (Discretize a,
                             MonadRandom (StateT b Identity)) =>
                            a -> ContingentClaim -> b -> Int -> Bool -> Double
    runSimulation modl ccs seed trials anti = runMC run seed (Observables [], 0)
       where
            run = simulateState modl (ccBasket ccs) trials anti

    quickSim :: Discretize a => a -> ContingentClaim -> Int -> Double
    quickSim mdl opts trials = runSimulation mdl opts (pureMT 500) trials False


getTrials :: MonteCarlo (Observables, Double) Int
getTrials = U.length <$> gets (obsHead . fst)


processClaimWithMap :: ContingentClaim' -> Map.Map Double Observables -> U.Vector Double
processClaimWithMap (ContingentClaim' _ c obs) m = c vals
    where 
        vals = map (\(t , g , f) -> U.map f . g $ m Map.! t) obs


