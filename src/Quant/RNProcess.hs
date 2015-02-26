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
    -- | Function to generate a forward.
    forwardRN :: a -> Double -> Double -> Double -> Double

data BasicForward = forall a . YieldCurve a => BasicForward a

instance ForwardGen BasicForward where
    forwardRN (BasicForward yc) s t1 t2 = s * disc yc t1 / disc yc t2