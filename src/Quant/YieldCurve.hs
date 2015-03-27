module Quant.YieldCurve (
    YieldCurve (..)
 ,  FlatCurve (..)
 ,  NetYC (..)
) where


{- | The 'YieldCurve' class defines the
basic operations of a yield curve.

Minimal complete definition: 'disc'.
-}
class YieldCurve a where
    -- | Calculate the discount factor for a given maturity.
    disc :: YieldCurve a => a -> Double -> Double

    -- | Calculate the forward rate between a t1 and t2
    forward :: YieldCurve a => a -> Double -> Double -> Double
    forward yc t1 t2 = (/(t2-t1)) $ log $ disc yc t1 / disc yc t2

    -- | Calculate the spot rate for a given maturity.
    spot :: YieldCurve a => a -> Double -> Double
    spot yc t = forward yc 0 t

-- |A flat curve is just a flat curve with one continuously 
-- compounded rate at all points on the curve.
data FlatCurve = FlatCurve Double

instance YieldCurve FlatCurve where
    disc (FlatCurve r) t = exp (-r*t)

data NetYC a = NetYC a a

instance YieldCurve a => YieldCurve (NetYC a) where
    disc (NetYC yc1 yc2) t = disc yc1 t / disc yc2 t