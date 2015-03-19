{-# LANGUAGE ExistentialQuantification #-}

module Quant.RNProcess (
    ForwardGen (..)
) where

import Quant.YieldCurve

{- | The 'ForwardGen' class generates risk-neutral
forwards for a given period.

Minimal complete definition: 'forwardRN'.
-}
class ForwardGen a where
    -- | Function to generate a forward multiplier.
    forwardRN :: a -> Double -> Double -> Double

data BasicForward = forall a . YieldCurve a => BasicForward a

instance ForwardGen BasicForward where
    forwardRN (BasicForward yc) t1 t2 =  forward yc t1 t2