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

black = Black 100 0.2 baseYC baseYC

opt = forwardContract 1

val = quickSim black [opt] 1000