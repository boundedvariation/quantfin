
module Quant.Models.Processes (
    ProcessSpec (..)
  , lognormal
) where

data ProcessSpec = ProcessSpec {
    procInit    :: Double
  , procGrowth  :: Double
  , procElapsed :: Double
}

lognormal :: ProcessSpec -> Double -> Double -> Double 
lognormal (ProcessSpec initVal r t) vol normRand = initVal * exp ( g + sig * normRand )
  where
    g   = (r - vol*vol/2) * t
    sig = vol * sqrt t

--cir :: ProcessSpec -> Double 
--cir (ProcessSpec initVal r t) vol normRand 