module Quant.VolSurf (
    VolSurf (..)
 ,  FlatSurf (..)
) where


{- | The 'VolSurf' class defines the
basic operations of a volatility surface.

Minimal complete definition: 'vol'.
-}
class VolSurf a where
    -- | Calculate the implied vol for a given strike/maturity.
    vol :: VolSurf a => a -> Double -> Double -> Double

    -- | Calculate the variance at a given strike/maturity.
    var :: VolSurf a => a -> Double -> Double -> Double
    var vs s t = v*v*t
        where v = vol vs s t

-- |A flat curve is just a flat curve with one continuously 
-- compounded rate at all points on the curve.
data FlatSurf = FlatSurf Double

instance VolSurf FlatSurf where
    vol (FlatSurf x) _ _ = x