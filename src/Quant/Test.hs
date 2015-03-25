module Quant.Test (
	baseYC
  ,	val
  , black
  , opt
	)
where

import Quant.MonteCarlo
import Quant.YieldCurve
import Quant.Models.Black

baseYC = FlatCurve 0.05

black = Black 100 0.0 baseYC baseYC

opt = vanillaOption Call 100 1.0

val = quickSim1 black [opt] 1