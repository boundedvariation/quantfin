{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}


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
  , MCVector
  , mcVecLen
  , constant
  ) where

import Quant.Time
import qualified Data.Vector.Unboxed as U

mcVecLen :: Int
mcVecLen = 1024
{-# INLINE mcVecLen #-}

constant :: U.Unbox a => a -> U.Vector a
constant x = U.replicate mcVecLen x
{-# INLINE constant #-}

type MCVector = U.Vector Double

-- | A CashFlow is just a time and an amount.
data CashFlow = CashFlow {
    cfTime   :: Time
  , cfAmount :: MCVector
}


-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)

-- | Single-observable container.
data Observables1 = Observables1 {-# UNPACK #-} !MCVector
-- | Two observable container.
data Observables2 = Observables2 {-# UNPACK #-} !MCVector !MCVector
-- | Three observable container.
data Observables3 = Observables3 {-# UNPACK #-} !MCVector !MCVector !MCVector
-- | Four observable container.
data Observables4 = Observables4 {-# UNPACK #-} !MCVector !MCVector !MCVector !MCVector
-- | Five observable container.
data Observables5 = Observables5 {-# UNPACK #-} !MCVector !MCVector !MCVector !MCVector !MCVector

class Obs1 a where
    get1 :: a -> MCVector

class (Obs1 a) => Obs2 a where
	get2 :: a -> MCVector

class (Obs2 a) => Obs3 a where
	get3 :: a -> MCVector

class (Obs3 a) => Obs4 a where
	get4 :: a -> MCVector

class (Obs4 a) => Obs5 a where
	get5 :: a -> MCVector

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