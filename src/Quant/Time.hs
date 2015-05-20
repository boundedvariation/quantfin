
module Quant.Time (
	Time(..)
  , timeDiff
  , timeOffset
  , timeFromZero
  ) where

data Time = Time Double deriving (Eq,Show,Ord)

timeDiff :: Time -> Time -> Double
timeDiff (Time x) (Time y) = y - x

timeOffset :: Time -> Double -> Time
timeOffset (Time x) y = Time (x+y)

timeFromZero :: Time -> Double
timeFromZero (Time x) = x