
module Quant.Models.Processes (
    ProcessSpec (..)
  , normal
  , lognormal
) where

data ProcessSpec = ProcessSpec {-# UNPACK #-} !Double !Double !Double

normal :: ProcessSpec -> Double -> Double -> Double
normal (ProcessSpec initVal r t) vol normRand = initVal + vol * normRand + r * t

lognormal :: ProcessSpec -> Double -> Double -> Double 
lognormal (ProcessSpec initVal r t) vol normRand = initVal * exp ( g + sig * normRand )
  where
    g   = (r - vol*vol/2) * t
    sig = vol * sqrt t

--cir :: ProcessSpec -> Double 
--cir (ProcessSpec initVal r t) vol normRand 