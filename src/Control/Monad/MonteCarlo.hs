{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.MonteCarlo (
	MonteCarlo
  ,	MonteCarloT
  , runMC
  , calc 
  , test)
where

import Data.Random
import Control.Monad.State
import Data.Functor.Identity
import System.Random.MWC

type MonteCarloT m a b = StateT a (RVarT m) b

type MonteCarlo a b = MonteCarloT Identity a b

calc :: (RandomSource m s) => s -> Double -> m Double
calc rnd initSt = runMC f rnd initSt 
	where f = do
		x <- get
		y <- lift stdNormalT
		z <- lift stdNormalT
		put $ x+y+z


runMC :: RandomSource m s => MonteCarloT m a b -> s -> a -> m a
runMC mc randState initState = runRVarT (execStateT mc initState) randState

test :: IO Double
test = do
	x <- create
	calc x 0.0