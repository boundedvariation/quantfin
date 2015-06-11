
module Quant.Types (
    CashFlow(..)
  , Observables1(..)
  , Observables2(..)
  , Observables3(..)
  , Observables4(..)
  , Observables5(..)
  , MCVector
  , vecLen
  , OptionType(..)
  , Obs1(..)
  , Obs2(..)
  , Obs3(..)
  , Obs4(..)
  , Obs5(..)
  ) where

import Quant.Time
import qualified Data.Vector.Unboxed as U

-- | A CashFlow is just a time and an amount.
data CashFlow a = CashFlow {
    cfTime   :: Time
  , cfAmount :: a
}

type MCVector = U.Vector Double

vecLen :: Int
vecLen = 1024

-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)

-- | Single-observable container.
data Observables1 a = Observables1 !a
-- | Two observable container.
data Observables2 a = Observables2 !a !a
-- | Three observable container.
data Observables3 a = Observables3 !a !a !a
-- | Four observable container.
data Observables4 a = Observables4 !a !a !a !a
-- | Five observable container.
data Observables5 a = Observables5 !a !a !a !a !a

instance Functor Observables1 where
  fmap f (Observables1 a) = Observables1 (f a)

instance Functor Observables2 where
  fmap f (Observables2 a b) = Observables2 (f a) (f b)

instance Functor Observables3 where
  fmap f (Observables3 a b c) = Observables3 (f a) (f b) (f c)

instance Functor Observables4 where
  fmap f (Observables4 a b c d) = Observables4 (f a) (f b) (f c) (f d)

instance Functor Observables5 where
  fmap f (Observables5 a b c d e) = Observables5 (f a) (f b) (f c) (f d) (f e)

class Obs1 a where
    get1 :: a b -> b

class (Obs1 a) => Obs2 a where
	get2 :: a b -> b

class (Obs2 a) => Obs3 a where
	get3 :: a b -> b

class (Obs3 a) => Obs4 a where
	get4 :: a b -> b

class (Obs4 a) => Obs5 a where
	get5 :: a b -> b

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