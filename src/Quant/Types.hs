
module Quant.Types (
    CashFlow(..)
  , Observables1(..)
  , Observables2(..)
  , Observables3(..)
  , Observables4(..)
  , Observables5(..)
  , OptionType(..)
  , Obs1(..)
  , Obs2(..)
  , Obs3(..)
  , Obs4(..)
  , Obs5(..)
  ) where

import Quant.Time

-- | A CashFlow is just a time and an amount.
data CashFlow = CashFlow {
    cfTime   :: Time
  , cfAmount :: Double
}


-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)

-- | Single-observable container.
data Observables1 = Observables1 {-# UNPACK #-} !Double
-- | Two observable container.
data Observables2 = Observables2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
-- | Three observable container.
data Observables3 = Observables3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
                                 {-# UNPACK #-} !Double
-- | Four observable container.
data Observables4 = Observables4 {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
                                 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
-- | Five observable container.
data Observables5 = Observables5 {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
                                 {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
                                 {-# UNPACK #-} !Double

class Obs1 a where
    get1 :: a -> Double

class (Obs1 a) => Obs2 a where
    get2 :: a -> Double

class (Obs2 a) => Obs3 a where
    get3 :: a -> Double

class (Obs3 a) => Obs4 a where
    get4 :: a -> Double

class (Obs4 a) => Obs5 a where
    get5 :: a -> Double

instance Obs1 Observables1 where
    get1 (Observables1 x) = x
    {-# INLINE get1 #-}

instance Obs1 Observables2 where
    get1 (Observables2 x _) = x
    {-# INLINE get1 #-}

instance Obs1 Observables3 where
    get1 (Observables3 x _ _) = x
    {-# INLINE get1 #-}

instance Obs1 Observables4 where
    get1 (Observables4 x _ _ _) = x
    {-# INLINE get1 #-}

instance Obs1 Observables5 where
    get1 (Observables5 x _ _ _ _) = x
    {-# INLINE get1 #-}

instance Obs2 Observables2 where
    get2 (Observables2 _ x) = x
    {-# INLINE get2 #-}

instance Obs2 Observables3 where
    get2 (Observables3 _ x _) = x
    {-# INLINE get2 #-}

instance Obs2 Observables4 where
    get2 (Observables4 _ x _ _) = x
    {-# INLINE get2 #-}

instance Obs2 Observables5 where
    get2 (Observables5 _ x _ _ _) = x
    {-# INLINE get2 #-}

instance Obs3 Observables3 where
    get3 (Observables3 _ _ x) = x
    {-# INLINE get3 #-}

instance Obs3 Observables4 where
    get3 (Observables4 _ _ x _) = x
    {-# INLINE get3 #-}

instance Obs3 Observables5 where
    get3 (Observables5 _ _ x _ _) = x
    {-# INLINE get3 #-}

instance Obs4 Observables4 where
    get4 (Observables4 _ _ _ x) = x
    {-# INLINE get4 #-}

instance Obs4 Observables5 where
    get4 (Observables5 _ _ _ x _) = x
    {-# INLINE get4 #-}

instance Obs5 Observables5 where
    get5 (Observables5 _ _ _ _ x) = x
    {-# INLINE get5 #-}