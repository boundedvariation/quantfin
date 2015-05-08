{-# LANGUAGE FlexibleContexts #-}


module Quant.MonteCarlo (
    -- * The MonteCarlo type.
    MonteCarlo
  , MonteCarloT
  , runMC 

  -- * The discretize typeclass.
  , Discretize(..)
  , OptionType(..)

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

-- | A monad transformer for Monte-Carlo calculations.
type MonteCarloT m s = StateT s (RVarT m)

-- | Wraps the Identity monad in the 'MonteCarloT' transformer.
type MonteCarlo s a = MonteCarloT Identity s a

-- | "Runs" a MonteCarlo calculation and provides the result of the computation.
runMC :: MonadRandom (StateT b Identity) => MonteCarlo s c  -- ^ Monte Carlo computation.
                                         -> b  -- ^ Initial state.
                                         -> s  -- ^ Initial random-generator state.
                                         -> c  -- ^ Final result of computation.
runMC mc randState initState = flip evalState randState $ sampleRVarTWith lift (evalStateT mc initState)


{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize', 'discounter', 'forwardGen' and 'evolve''.
-}
class Discretize a where

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a => a   -- ^ Model
                               -> MonteCarlo (MCObservables, Double) ()

    -- | Evolves the internal states of the MC variables between two times.
    evolve :: Discretize a => a        -- ^ Model
                           -> Double   -- ^ time to evolve to
                           -> Bool     -- whether or not to use flipped variates
                           -> MonteCarlo (MCObservables, Double) ()
    evolve mdl t2 anti = do
        (_, t1) <- get
        let ms = maxStep mdl
        if t2 == t1 then return () else
          if (t2-t1) < ms then 
              evolve' mdl t2 anti
          else do
              evolve' mdl (t1 + ms) anti
              evolve mdl t2 anti

    -- | Stateful discounting function, takes a model and a time, and returns a vector of results.
    discounter :: Discretize a => a -> Double -> MonteCarlo (MCObservables, Double) Double

    -- | Stateful forward generator for a given model at a certain time.
    forwardGen :: Discretize a => a -> Double -> MonteCarlo (MCObservables, Double) Double

    -- | Internal function to evolve a model to a given time.
    evolve' :: Discretize a => a      -- ^ model
                            -> Double -- ^ time to evolve to
                            -> Bool   -- ^ whether or not to use flipped variates
                            -> MonteCarlo (MCObservables, Double) () -- ^ computation result

    -- | Determines the maximum size time-step for discretization purposes. Defaults to 1/250.
    maxStep :: Discretize a => a -> Double
    maxStep _ = 1/250

    -- | Perform a simulation of a compiled basket of contingent claims.
    simulateState :: Discretize a => 
           a                       -- ^ model
        -> ContingentClaim         -- ^ compilied basket of claims
        -> Int                     -- ^ number of trials
        -> Bool                    -- ^ antithetic?
        -> MonteCarlo (MCObservables, Double) Double -- ^ computation result
    simulateState modl (ContingentClaim ccb) trials anti = avg <$> replicateM trials singleTrial
          where 
            singleTrial = initialize modl >> 
                            process (0 :: Double) Map.empty ccb []


            process discCFs obsMap (cc:ccs) (cf:cfs) = 
                let ccTime = getTime cc
                    nearestCFTime = cfTime cf in
                if ccTime > nearestCFTime then do
                    evolve modl nearestCFTime anti
                    d <- discounter modl nearestCFTime
                    process (discCFs+d*cfAmount cf) obsMap (cc:ccs) cfs
                else do
                    evolve modl ccTime anti
                    case cc of
                      Monitor t -> do
                        evolve modl t anti
                        obs <- gets fst
                        let obsMap' = Map.insert ccTime obs obsMap
                        process discCFs obsMap' ccs (cf:cfs)
                      Payout t f -> evolve modl t anti >> 
                          process discCFs obsMap ccs (insertCF (f obsMap) (cf:cfs))

            process discCFs obsMap (cc:ccs) [] = case cc of
                      Monitor t -> do
                        evolve modl t anti
                        obs <- gets fst
                        let obsMap' = Map.insert t obs obsMap
                        process discCFs obsMap' ccs []
                      Payout t f -> evolve modl t anti >>
                          process discCFs obsMap ccs [f obsMap]

            process discCFs obsMap [] (cf:cfs) = do
                evolve modl (cfTime cf) anti
                d <- discounter modl $ cfTime cf
                process (discCFs+d*cfAmount cf) obsMap [] cfs


            insertCF (CashFlow t amt) ((CashFlow t' amt'):cfs)
              | t > t' = (CashFlow t' amt') : insertCF (CashFlow t amt) cfs
              | otherwise = (CashFlow t amt) : CashFlow t' amt' : cfs
            insertCF cf [] = [cf]

            avg v = sum v / fromIntegral trials

    -- | Runs a simulation for a 'ContingentClaim'.
    runSimulation :: (Discretize a,
                             MonadRandom (StateT b Identity)) =>
                                a                  -- ^ model
                             -> ContingentClaim    -- ^ claims to value
                             -> b                  -- ^ initial random state
                             -> Int                -- ^ trials
                             -> Bool               -- ^ whether to use antithetic variables
                             -> Double             -- ^ final value
    runSimulation modl ccs seed trials anti = runMC run seed (Observables [], 0)
       where
            run = simulateState modl ccs trials anti

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