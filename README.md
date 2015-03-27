# quantfin
Quant Finance in Pure Haskell.

Initially I'm focusing on a Monte Carlo engine, but plenty more to come.

```haskell

import Quant.MonteCarlo
import Quant.YieldCurve
import Quant.ContingentClaim
import Quant.Models.Black

baseYC = FlatCurve 0.05 --create a flat yield curve with a 5% rate

black = Black 
			100     --initial stock price
			0.2     --volatility
			baseYC  --forward generator
			baseYC  --discount rate

opt = vanillaOption Put 100 1 --make a vanilla put, struck at 100, maturing at time 1

val = quickSim black opt 10000 --Run a Monte Carlo on opt in a a black model with 10000 trials
							   --Returns 5.448

opt' = multiplier 100 
	$ vanillaOption Call 100 1 ++ short (vanillaOption Call 120 1) --Make a call spread with a 100 unit notional

val' = 

```