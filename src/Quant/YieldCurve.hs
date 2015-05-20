module Quant.YieldCurve (
    YieldCurve (..)
 ,  FlatCurve (..)
 ,  NetYC (..)
) where

import Quant.Time

{- | The 'YieldCurve' class defines the
basic operations of a yield curve.

Minimal complete definition: 'disc'.
-}
class YieldCurve a where
    -- | Calculate the discount factor for a given maturity.
    disc :: YieldCurve a => a -> Time -> Double

    -- | Calculate the forward rate between a t1 and t2
    forward :: YieldCurve a => a -> Time -> Time -> Double
    forward yc t1 t2 = (/(timeFromZero t2-timeFromZero t1)) $ log $ disc yc t1 / disc yc t2
    {-# INLINE forward #-}

    -- | Calculate the spot rate for a given maturity.
    spot :: YieldCurve a => a -> Time -> Double
    spot yc t = forward yc (Time 0) t
    {-# INLINE spot #-}

-- |A flat curve is just a flat curve with one continuously 
-- compounded rate at all points on the curve.
data FlatCurve = FlatCurve Double

instance YieldCurve FlatCurve where
    disc (FlatCurve r) t = exp (-r*timeFromZero t)

-- | 'YieldCurve' that represents the difference between two 'YieldCurve's.
data NetYC a = NetYC a a

instance YieldCurve a => YieldCurve (NetYC a) where
    disc (NetYC yc1 yc2) t = disc yc1 t / disc yc2 t