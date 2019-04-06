
module Quant.Time (
    Time(..)
  , timeDiff
  , timeOffset
  , timeFromZero
  ) where

data Time = Time {-# UNPACK #-} !Double deriving (Eq,Show,Ord)

timeDiff :: Time -> Time -> Double
timeDiff (Time x) (Time y) = y - x
{-# INLINE timeDiff #-}

timeOffset :: Time -> Double -> Time
timeOffset (Time x) y = Time (x+y)
{-# INLINE timeOffset #-}

timeFromZero :: Time -> Double
timeFromZero (Time x) = x
{-# INLINE timeFromZero #-}
