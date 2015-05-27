{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


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
import Quant.Time
import Quant.Types
import Data.RVar
import Data.Foldable (foldl')
import System.Random.Mersenne.Pure64
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U

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
class Discretize a b | a -> b where

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a b => a   -- ^ Model
                                 -> MonteCarlo (b, Time) ()

    -- | Evolves the internal states of the MC variables between two times.
    evolve :: Discretize a b => a            -- ^ Model
                             -> Time         -- ^ time to evolve to
                             -> Bool         -- whether or not to use flipped variates
                             -> MonteCarlo (b, Time) ()
    evolve mdl t2 anti = do
        (_, t1) <- get
        let ms = maxStep mdl
        unless (t2==t1) $
          if timeDiff t1 t2 < ms then 
              evolve' mdl t2 anti
          else do
              evolve' mdl (timeOffset t1 ms) anti
              evolve mdl t2 anti

    -- | Non-stateful discounting function...might need to find a better place to put this.
    discount :: Discretize a b => a -> Time -> MonteCarlo (b, Time) MCVector

    -- | Stateful forward generator for a given model at a certain time.
    forwardGen :: Discretize a b => a -> Time -> MonteCarlo (b, Time) MCVector

    -- | Internal function to evolve a model to a given time.
    evolve' :: Discretize a b => a          -- ^ model
                              -> Time       -- ^ time to evolve to
                              -> Bool       -- ^ whether or not to use flipped variates
                              -> MonteCarlo (b, Time) () -- ^ computation result

    -- | Determines the maximum size time-step for discretization purposes. Defaults to 1/250.
    maxStep :: Discretize a b => a -> Double
    maxStep _ = 1/250
    {-# INLINE maxStep #-}

    -- | Perform a simulation of a compiled basket of contingent claims.
    simulateState :: Discretize a b => 
           a                              -- ^ model
        -> ContingentClaim b              -- ^ compilied basket of claims
        -> Int                            -- ^ number of trials
        -> Bool                           -- ^ antithetic?
        -> MonteCarlo (b, Time) Double    -- ^ computation result
    simulateState modl (ContingentClaim ccb) trials anti = avg <$> U.replicateM (trials `div` mcVecLen) singleTrial
          where 
            singleTrial = do
              initialize modl
              x <- process (U.replicate mcVecLen 0) Map.empty ccb []
              return $ U.sum x / fromIntegral mcVecLen


            process discCFs obsMap c@(CCProcessor t mf:ccs) allcfs@(CashFlow cft amt:cfs) = 
              if t > cft then do
                  evolve modl cft anti
                  d <- discount modl cft
                  process (discCFs+d*amt) obsMap c cfs
              else do
                  evolve modl t anti
                  obs <- gets fst
                  let obsMap' = Map.insert t obs obsMap
                  case mf of
                    Nothing -> process discCFs obsMap' ccs allcfs
                    Just f -> let newCFs = map ($obsMap') f
                                  insertCFList xs cfList = foldl' (flip insertCF) cfList xs in
                        process discCFs obsMap' ccs (insertCFList newCFs allcfs)

            process discCFs obsMap (CCProcessor t mf:ccs) [] = do
              evolve modl t anti
              obs <- gets fst
              let obsMap' = Map.insert t obs obsMap
              case mf of
                Nothing -> process discCFs obsMap' ccs []
                Just f -> let newCFs = map ($obsMap') f
                              insertCFList xs cfList = foldl' (flip insertCF) cfList xs in
                        process discCFs obsMap' ccs (insertCFList newCFs [])                       

            process discCFs obsMap [] (cf:cfs) = do
              evolve modl (cfTime cf) anti
              d <- discount modl $ cfTime cf
              process (discCFs+d*cfAmount cf) obsMap [] cfs

            process discCFs _ _ _ = return $! discCFs

            insertCF (CashFlow t amt) (CashFlow t' amt':cfs)
              | t > t' = CashFlow t' amt' : insertCF (CashFlow t amt) cfs
              | otherwise = CashFlow t amt : CashFlow t' amt' : cfs
            insertCF cf [] = [cf]

            avg v = U.sum v / fromIntegral (trials `div` mcVecLen)

    -- | Runs a simulation for a 'ContingentClaim'.
    runSimulation :: (Discretize a b,
                             MonadRandom (StateT c Identity)) =>
                                a                             -- ^ model
                             -> ContingentClaim b             -- ^ claims to value
                             -> c                             -- ^ initial random state
                             -> Int                           -- ^ trials
                             -> Bool                          -- ^ whether to use antithetic variables
                             -> Double                        -- ^ final value
    runSimulation modl ccs seed trials anti = runMC run seed (undefined, Time 0)
       where
            run = simulateState modl ccs trials anti

    -- | Like 'runSimulation', but splits the trials in two and does antithetic variates.
    runSimulationAnti :: (Discretize a b,
                             MonadRandom (StateT c Identity)) =>
                            a -> ContingentClaim b -> c -> Int -> Double
    runSimulationAnti modl ccs seed trials = (runSim True + runSim False) / 2
        where runSim x = runSimulation modl ccs seed (trials `div` 2) x

    -- | 'runSimulation' with a default random number generator.
    quickSim :: Discretize a b => a -> ContingentClaim b -> Int -> Double
    quickSim mdl opts trials = runSimulation mdl opts (pureMT 500) trials False

    -- | 'runSimulationAnti' with a default random number generator.
    quickSimAnti :: Discretize a b => a -> ContingentClaim b -> Int -> Double
    quickSimAnti mdl opts trials = runSimulationAnti mdl opts (pureMT 500) trials