{-# LANGUAGE FlexibleInstances #-}

module Quant.Date (
    Time (..)
 ,  SimpleTime (..)
) where


{- | The 'Time' class defines the
basic operations of a volatility surface.

Minimal complete definition: 'vol'.
-}
class Ord a => Time a where
    -- | Calculate the time in years between two times.
    timeDiff :: Time a => a -> a -> Double

    currentTime :: Time a => a

data SimpleTime = SimpleTime Double deriving (Eq, Show, Ord)

instance Time SimpleTime where
    timeDiff (SimpleTime x) (SimpleTime y) = y - x
    currentTime = SimpleTime 0