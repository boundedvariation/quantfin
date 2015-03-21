{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.MonteCarlo (
    MonteCarlo
  , MonteCarloT
  , runMC )
where

import Data.Random
import Control.Monad.State
import Data.Functor.Identity

type MonteCarloT m s a = StateT s (RVarT m) a

type MonteCarlo s a = MonteCarloT Identity s a

runMC :: RandomSource m s => MonteCarloT m a b -> s -> a -> m a
runMC mc randState initState = runRVarT (execStateT mc initState) randState