{-# LANGUAGE FlexibleContexts #-}


module Quant.MCTypes (
    -- * The MonteCarlo type.
    MonteCarlo
  , MonteCarloT
  , MCProducer
  , MCConsumer
  , runMC 
  , MCObservables
  , OptionType(..)
  , ContingentClaim
  )
where

import Pipes
import Data.Functor.Identity
import Control.Monad.State
import Data.RVar
import qualified Data.Vector.Unboxed as U

type MCObservables = Observables (U.Vector Double)

type MCProducer a = Producer 
    MCObservables 
    (MonteCarloT Identity (MCObservables, Double)) 
    a

type MCConsumer a = Consumer 
    MCObservables
    (MonteCarloT Identity (MCObservables, Double)) 
    a


type MCPipe a = Pipe
    MCObservables
    (U.Vector Double)
    (MonteCarloT Identity (MCObservables, Double))
    a

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


-- | 'ContingentClaim' is just a list of the underlying 'ContingentClaim''s.
type ContingentClaim = [MCConsumer (U.Vector Double)]

-- | Observables are the observables available in a Monte Carlo simulation.
--Most basic MCs will have one observables (Black-Scholes) whereas more
--complex ones will have multiple (i.e. Heston-Hull-White).
data Observables a = Observables [a] deriving (Eq, Show)

-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)