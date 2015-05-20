
module Quant.Types (
    CashFlow(..)
  , Observables(..)
  , MCObservables
  , OptionType(..)
  ) where

import Quant.Time

data CashFlow = CashFlow {
    cfTime   :: Time
  , cfAmount :: Double
}

-- | Observables are the observables available in a Monte Carlo simulation.
--Most basic MCs will have one observables (Black-Scholes) whereas more
--complex ones will have multiple (i.e. Heston-Hull-White).
data Observables a = Observables { obsGet :: [a] } deriving (Show)

type MCObservables = Observables Double

-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)
