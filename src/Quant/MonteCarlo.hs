{-# LANGUAGE FlexibleContexts #-}


module Quant.MonteCarlo (
    -- * The MonteCarlo type.
    MonteCarlo
  , MonteCarloT
  , runMC 
  , MCObservables

  -- * The discretize typeclass.
  , Discretize(..)
  , OptionType(..)

  , getTrials

  )
where

import Quant.ContingentClaim
import Quant.MCTypes
import Pipes
import Data.Random
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.RVar
import System.Random.Mersenne.Pure64
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U


{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize', 'discounter', 'forwardGen' and 'evolve''.
-}
class Discretize a where

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a => a   -- ^ Model
                               -> Int -- ^ number of trials
                               -> MCProducer ()

    -- | Evolves the internal states of the MC variables between two times.
    evolve :: Discretize a => a        -- ^ Model
                           -> Double   -- ^ time to evolve to
                           -> Bool     -- whether or not to use flipped variates
                           -> MCProducer ()
    evolve mdl t2 anti = do
        (_, t1) <- lift get
        let ms = maxStep mdl
        if (t2-t1) < ms then 
            evolve' mdl t2 anti
        else do
            evolve' mdl (t1 + ms) anti
            evolve mdl t2 anti

    -- | Stateful discounting function, takes a model and a time, and returns a vector of results.
    discounter :: Discretize a => a -> Double -> MCProducer ()

    -- | Stateful forward generator for a given model at a certain time.
    forwardGen :: Discretize a => a -> Double -> MCProducer ()

    -- | Internal function to evolve a model to a given time.
    evolve' :: Discretize a => a      -- ^ model
                            -> Double -- ^ time to evolve to
                            -> Bool   -- ^ whether or not to use flipped variates
                            -> MCProducer () -- ^ computation result

    -- | Determines the maximum size time-step for discretization purposes. Defaults to 1/250.
    maxStep :: Discretize a => a -> Double
    maxStep _ = 1/250

    -- | Perform a simulation of a compiled basket of contingent claims.
    simulateState :: Discretize a => 
            a                      -- ^ model
        ->  ContingentClaimBasket  -- ^ compilied basket of claims
        -> Int                     -- ^ number of trials
        -> Bool                    -- ^ antithetic?
        -> MonteCarlo (MCObservables, Double) Double -- ^ computation result
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

    -- | Runs a simulation for a 'ContingentClaim'.
    runSimulation :: (Discretize a,
                             MonadRandom (StateT b Identity)) =>
                                a                -- ^ model
                             -> ContingentClaim  -- ^ claims to value
                             -> b                -- ^ initial random state
                             -> Int              -- ^ trials
                             -> Bool             -- ^ whether to use antithetic variables
                             -> Double           -- ^ final value
    runSimulation modl ccs seed trials anti = runMC run seed (Observables [], 0)
       where
            run = simulateState modl (ccBasket ccs) trials anti

    -- | Like 'runSimulation', but splits the trials in two and does antithetic variates.
    runSimulationAnti :: (Discretize a,
                             MonadRandom (StateT b Identity)) =>
                            a -> ContingentClaim -> b -> Int -> Double
    runSimulationAnti modl ccs seed trials = (runSim True + runSim False) / 2
        where runSim = runSimulation modl ccs seed (trials `div` 2)

    -- | 'runSimulation' with a default random number generator.
    quickSim :: Discretize a => a -> ContingentClaim -> Int -> Double
    quickSim mdl opts trials = runSimulation mdl opts (pureMT 500) trials False

    -- | 'runSimulationAnti' with a default random number generator.
    quickSimAnti :: Discretize a => a -> ContingentClaim -> Int -> Double
    quickSimAnti mdl opts trials = runSimulationAnti mdl opts (pureMT 500) trials

-- | Utility function to get the number of trials.
getTrials :: MonteCarlo (MCObservables, Double) Int
getTrials = U.length <$> gets (obsHead . fst)


processClaimWithMap :: ContingentClaim' -> Map.Map Double MCObservables -> U.Vector Double
processClaimWithMap (ContingentClaim' _ c obs) m = c vals
    where 
        vals = map (\(ObservablePuller t g f) -> U.map f . g $ m Map.! t) obs


