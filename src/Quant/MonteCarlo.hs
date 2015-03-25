{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Quant.MonteCarlo (
    MonteCarlo
  , MonteCarloT
  , runMC 
  , ContingentClaim(..)
  , Discretize(..)
  , ExternalState
  , simulate1Observable
  )
where

import Data.Random
import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as U

type MonteCarloT m s = StateT s (RVarT m)

type MonteCarlo s a = MonteCarloT Identity s a

runMC :: RandomSource m s => MonteCarloT m a b -> s -> a -> m a
runMC mc randState initState = runRVarT (execStateT mc initState) randState

data ContingentClaim = ContingentClaim {
    payoutTime :: Double
  , collector :: U.Vector Double -> Double
  , observations :: [(Double, Double -> Double)]
}

type ExternalState = (U.Vector Double, Double)

{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize', 'discounter' and 'evolve'.
-}
class Discretize a b where
    -- | Represents the internal state of the function.

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a b => a -> Int -> MonteCarlo (b, ExternalState) ()

    -- | Evolves the internal states of the MC variables between two times.
    -- | First variable is t0, second is t1
    evolve :: Discretize a b => a -> Double -> Double -> MonteCarlo (b, ExternalState) ()

    -- | Evolves to terminal values.
    evolveTerminal :: Discretize a b => a -> Double -> Int -> MonteCarlo (b, ExternalState) ()
    evolveTerminal model t steps = evolveTimeSteps model (init vals) (tail vals)
        where 
          dt = t / fromIntegral steps
          vals = take (steps+1) $ iterate (+dt) 0.0
      
    -- | Evolves based on a series of of start/end times.
    evolveTimeSteps :: Discretize a b => a -> [Double] -> [Double] -> MonteCarlo (b, ExternalState) ()
    evolveTimeSteps model t0 t1 = forM_ vals $ uncurry $ evolve model
      where vals = zip t0 t1

    discounter :: Discretize a b => a -> Double -> MonteCarlo (b, ExternalState) (U.Vector Double)

    forwardGen :: Discretize a b => a -> Double -> Double -> MonteCarlo (b, ExternalState) (U.Vector Double)

type SingleObservable a = MonteCarlo (U.Vector Double, ExternalState) a

simulate1Observable :: Discretize a (U.Vector Double) => 
    a -> ContingentClaim -> Int ->
    SingleObservable (U.Vector Double)
simulate1Observable modl (ContingentClaim tmat c obs) trials = do
    initialize modl trials
    obsFuncResults <- forM obs $ \(t, obfunc) -> do
        currentT <- gets (snd . snd)
        evolve modl currentT t
        vals <- gets fst
        return $ U.map obfunc vals
    currentT <- gets (snd . snd)
    evolve modl currentT tmat
    d <- discounter modl tmat
    return $ U.zipWith (*) d $ U.fromList $ map c obsFuncResults