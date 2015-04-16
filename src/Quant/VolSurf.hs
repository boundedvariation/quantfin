module Quant.VolSurf (
    VolSurf (..)
 ,  FlatSurf (..)
 ,  GridSurf (..)
) where

import Quant.Types
import Quant.Math.Interpolation
import Quant.YieldCurve
import qualified Data.Map as M

{- | The 'VolSurf' class defines the
basic operations of a volatility surface.

Minimal complete definition: 'vol'.
-}
class VolSurf a where
    -- | Calculate the implied vol for a given strike/maturity.
    vol :: VolSurf a => a -> Double -> TimeOffset -> Double

    -- | Calculate the variance at a given strike/maturity.
    var :: VolSurf a => a -> Double -> TimeOffset -> Double
    var vs s t = v*v*t
        where v = vol vs s t

    -- | Calculates Dupire local vol for a given strike/maturity/forward generating yield curve.
    localVol :: (VolSurf a, YieldCurve b) => a           -- ^ Volatility surface
                                          -> Double      -- ^ Initial stock price
                                          -> b           -- ^ 'YieldCurve' to generate forwards
                                          -> Double      -- ^ Current stock level
                                          -> TimeOffset  -- ^ Time
                                          -> Double      -- ^ Local volatility
    localVol v s0 rcurve k t 
        | w==0.0 || solution<0.0 = sqrt dwdt
        | otherwise = sqrt solution
            where
                dr = disc rcurve t
                f = s0/dr
                y = log $ k/f
                dy = 1.0E-6
                kp = k*exp dy
                km = k/exp dy
                [w, wp, wm] = map (\x->var v (x/s0) t) [k, kp, km]
                dwdy = (wp-wm)/2.0/dy
                d2wdy2 = (wp-2.0*w+wm)/dy/dy
                dt = min 0.0001 (t/2.0)
                dwdt = let 
                    strikept = k*dr/drpt
                    strikemt = k*dr/drmt
                    drpt = disc rcurve $ t+dt
                    drmt = disc rcurve $ t-dt
                        in case t of
                            0 -> (var v (strikept/s0) (t+dt) -w)/dt
                            _ -> (var v (strikept/s0) (t+dt)-var v (strikemt/s0) (t-dt))/2.0/dt                       
                solution = dwdt/(1.0-y/w*dwdy+0.25*(-0.25-1.0/w+y*y/w/w)*dwdy*dwdy+0.5*d2wdy2)

-- |A flat surface has one volatility at all times/maturities.
data FlatSurf = FlatSurf Double

instance VolSurf FlatSurf where
    vol (FlatSurf x) _ _ = x

data GridSurf = GridSurf {
    gridStrikes             :: [Double]
  , gridMaturities          :: [TimeOffset]
  , gridQuotes              :: M.Map (Double, TimeOffset) Double
  , gridStrikeInterpolator  :: Interpolator1d
  , gridTimeInterpolator    :: Interpolator1d
}

instance VolSurf GridSurf where
    vol (GridSurf sts mats quotes vInterp tInterp) strike t = tInterp mats interpolatedVs t
        where
            interpolatedVs = map (\k -> vInterp sts k strike) $ map allForT mats
            allForT t' = map (\ x -> quotes M.! (x, t')) sts