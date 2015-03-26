{-# LANGUAGE FlexibleContexts #-}


module Quant.MonteCarlo (
    MonteCarlo
  , MonteCarloT
  , runMC 
  --, runMCT
  , ContingentClaim(..)
  , ContingentClaimBasket(..)
  , Discretize(..)
  , simulate1ObservableState
  , OptionType(..)
  , vanillaOption
  , binaryOption
  , spreadOption
  , multiplier
  , short
  , forwardContract
  , runSimulation1
  , ccBasket
  , quickSim1
  , processClaimWithMap
  , Observables(..)
  , obsPull
  )
where

import Data.Random
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.List
import Data.RVar
import Data.Ord
import Debug.Trace
import System.Random.Mersenne.Pure64
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U

type MonteCarloT m s = StateT s (RVarT m)

type MonteCarlo s a = MonteCarloT Identity s a

runMC :: MonadRandom (StateT b Identity) => MonteCarlo s c -> b -> s -> c
runMC mc randState initState = flip evalState randState $ sampleRVarTWith lift (evalStateT mc initState)

data ContingentClaim = ContingentClaim {
    payoutTime   :: Double
  , collector    :: U.Vector Double -> Double
  , observations :: [(Double, Double -> Double)]
}

data OptionType = Put | Call deriving (Eq,Show)

{- | The 'Discretize' class defines those
models on which Monte Carlo simulations
can be performed.

Minimal complete definition: 'initialize', 'discounter', 'forwardGen' and 'evolve'.
-}

data Observables = Observables [U.Vector Double] deriving (Eq, Show)

class Discretize a where
    -- | Represents the internal state of the function.

    -- | Initializes a Monte Carlo simulation for a given number of runs.
    initialize :: Discretize a => a -> Int -> MonteCarlo (Observables, Double) ()

    -- | Evolves the internal states of the MC variables between two times.
    -- | First variable is t0, second is t1
    evolve :: Discretize a => a -> Double -> MonteCarlo (Observables, Double) ()
    evolve mdl t2 = do
        (_, t1) <- get
        let ms = minStep mdl
        if (t2-t1) < ms then 
            evolve' mdl t2
        else do
            evolve' mdl (t1 + ms)
            evolve mdl t2

    discounter :: Discretize a => a -> Double -> MonteCarlo (Observables, Double) (U.Vector Double)

    forwardGen :: Discretize a => a -> Double -> MonteCarlo (Observables, Double) (U.Vector Double)

    evolve' :: Discretize a => a -> Double -> MonteCarlo (Observables, Double) ()

    minStep :: Discretize a => a -> Double --need to think of a way to get the b out with introducing a mess.
    minStep _ = 1/250


simulate1ObservableState :: Discretize a => 
    a -> ContingentClaimBasket -> Int ->
    MonteCarlo (Observables, Double) Double
simulate1ObservableState modl (ContingentClaimBasket cs ts) trials = do
    initialize modl trials
    avg <$> process Map.empty (U.replicate trials 0) cs ts
        where 
            process m cfs ccs@(c@(ContingentClaim t _ _):cs') (obsT:ts') = do
                Observables (vals:_) <- gets fst
                if t > obsT then do
                    evolve modl obsT
                    let m' = Map.insert obsT vals m
                    process m' cfs ccs ts'
                else 
                    if obsT == t then do
                        evolve modl obsT
                        let m' = Map.insert obsT vals m
                            cfs' = processClaimWithMap c m'
                        d <- discounter modl obsT
                        let cfs'' = cfs' |*| d
                        process m' (cfs |+| cfs'') cs' ts'
                    else do
                        evolve modl t
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

processClaimWithMap :: ContingentClaim -> Map.Map Double (U.Vector Double) -> U.Vector Double
processClaimWithMap (ContingentClaim _ c obs) m = U.fromList $ map c vals
    where vals = map (\(t , f) -> U.map f $ m Map.! t) obs

vanillaPayout :: OptionType -> Double -> Double -> Double
vanillaPayout pc strike x = case pc of
    Put  -> max (strike - x) 0
    Call -> max (x - strike) 0

binaryPayout :: OptionType -> Double -> Double -> Double -> Double
binaryPayout pc strike amount x = case pc of
    Put  -> if strike > x then amount else 0
    Call -> if x > strike then amount else 0

terminalOnly :: Double -> (Double -> Double) -> ContingentClaim
terminalOnly t f = ContingentClaim t U.head [(t, f)]

vanillaOption :: OptionType -> Double -> Double -> ContingentClaim
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

binaryOption :: OptionType -> Double -> Double -> Double -> ContingentClaim
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional c@(ContingentClaim _ collFct _) = c { collector = \x -> notional * collFct x }

short :: ContingentClaim -> ContingentClaim
short = multiplier (-1)

forwardContract :: Double -> ContingentClaim
forwardContract t = terminalOnly t id

spreadPayout :: OptionType -> Double -> Double -> Double -> Double
spreadPayout pc lowStrike highStrike x = case pc of
    Put  | x > highStrike -> 0
         | x < lowStrike  -> highStrike - lowStrike
         | otherwise      -> highStrike - x
    Call | x > highStrike -> highStrike - lowStrike
         | x < lowStrike  -> 0
         | otherwise      -> x - lowStrike

spreadOption :: OptionType -> Double -> Double -> Double -> ContingentClaim
spreadOption pc lowStrike highStrike t = terminalOnly t $ spreadPayout pc lowStrike highStrike

data ContingentClaimBasket = ContingentClaimBasket [ContingentClaim] [Double]

ccBasket :: [ContingentClaim] -> ContingentClaimBasket
ccBasket ccs = ContingentClaimBasket (sortBy (comparing payoutTime) ccs) monitorTimes
    where monitorTimes = sort $ nub $ concatMap (map fst . observations) ccs

runSimulation1 :: (Discretize a,
                         MonadRandom (StateT b Identity)) =>
                        a -> [ContingentClaim] -> b -> Int -> Double
runSimulation1 modl ccs seed trials = runMC run seed (Observables [U.empty], 0)
   where
        run = simulate1ObservableState modl (ccBasket ccs) trials

quickSim1 :: Discretize a => a -> [ContingentClaim] -> Int -> Double
quickSim1 mdl opts trials = runSimulation1 mdl opts (pureMT 500) trials

obsPull :: Observables -> U.Vector Double
obsPull (Observables (x:_)) = x