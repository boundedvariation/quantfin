
module Quant.Types (
    CashFlow(..)
  , Observables1(..)
  , Observables2(..)
  , Observables3(..)
  , Observables4(..)
  , OptionType(..)
  , Obs1(..)
  , Obs2(..)
  , Obs3(..)
  , Obs4(..)
  ) where

import Quant.Time

data CashFlow = CashFlow {
    cfTime   :: Time
  , cfAmount :: Double
}


-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)

data Observables1 = Observables1 {-# UNPACK #-} !Double
data Observables2 = Observables2 {-# UNPACK #-} !Double !Double
data Observables3 = Observables3 {-# UNPACK #-} !Double !Double !Double
data Observables4 = Observables4 {-# UNPACK #-} !Double !Double !Double !Double

class Obs1 a where
    get0 :: a -> Double

class (Obs1 a) => Obs2 a where
	get1 :: a -> Double

class (Obs2 a) => Obs3 a where
	get2 :: a -> Double

class (Obs3 a) => Obs4 a where
	get3 :: a -> Double

instance Obs1 Observables1 where
	get0 (Observables1 x) = x

instance Obs1 Observables2 where
	get0 (Observables2 x _) = x

instance Obs1 Observables3 where
	get0 (Observables3 x _ _) = x

instance Obs1 Observables4 where
	get0 (Observables4 x _ _ _) = x

instance Obs2 Observables2 where
	get1 (Observables2 _ x) = x

instance Obs2 Observables3 where
	get1 (Observables3 _ x _) = x

instance Obs2 Observables4 where
	get1 (Observables4 _ x _ _) = x

instance Obs3 Observables3 where
	get2 (Observables3 _ _ x) = x

instance Obs3 Observables4 where
	get2 (Observables4 _ _ x _) = x

instance Obs4 Observables4 where
	get3 (Observables4 _ _ _ x) = x