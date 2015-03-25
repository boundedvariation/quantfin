{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Quant.MonteCarlo (
    MonteCarlo
  , MonteCarloT
  , runMC 
  , ContingentClaim(..)
  , Discretize(..)
  , simulate1Observable
  , OptionType
  , vanillaOption
  , binaryOption
  , multiplier
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

data OptionType = Put | Call deriving (Eq,Show)

{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize', 'discounter', 'forwardGen' and 'evolve'.
-}
class Discretize a b where
    -- | Represents the internal state of the function.

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a b => a -> Int -> MonteCarlo (b, Double) ()

    -- | Evolves the internal states of the MC variables between two times.
    -- | First variable is t0, second is t1
    evolve :: Discretize a b => a -> Double -> MonteCarlo (b, Double) ()

    discounter :: Discretize a b => a -> Double -> MonteCarlo (b, Double) (U.Vector Double)

    forwardGen :: Discretize a b => a -> Double -> MonteCarlo (b, Double) (U.Vector Double)

type SingleObservable a = MonteCarlo (U.Vector Double, Double) a

simulate1Observable :: Discretize a (U.Vector Double) => 
    a -> ContingentClaim -> Int ->
    SingleObservable (U.Vector Double)
simulate1Observable modl (ContingentClaim tmat c obs) trials = do
    initialize modl trials
    obsFuncResults <- forM obs $ \(t, obfunc) -> do
        evolve modl t
        vals <- gets fst
        return $ U.map obfunc vals
    evolve modl tmat
    d <- discounter modl tmat
    return $ U.zipWith (*) d $ U.fromList $ map c obsFuncResults

vanillaPayout :: OptionType -> Double -> Double -> Double
vanillaPayout pc strike x | pc == Put  = max (x-strike) 0
                          | otherwise  = max (strike-x) 0

binaryPayout :: OptionType -> Double -> Double -> Double -> Double
binaryPayout pc strike amount x | pc == Put  = if strike > x then amount else 0
                                | otherwise  = if x > strike then amount else 0

terminalOnly :: Double -> (Double -> Double) -> ContingentClaim
terminalOnly t f = ContingentClaim t U.head [(t, f)]

vanillaOption :: OptionType -> Double -> Double -> ContingentClaim
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

binaryOption :: OptionType -> Double -> Double -> Double -> ContingentClaim
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

multiplier :: ContingentClaim -> Double -> ContingentClaim
multiplier c@(ContingentClaim _ collFct _) notional = c { collector = \x -> notional * collFct x }