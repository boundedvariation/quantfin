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
			baseYC  --discount function

--make a vanilla put, struck at 100, maturing at time 1
opt = vanillaOption Put 100 1 

--Run a Monte Carlo on opt in a a black model with 10000 trials
--Returns 5.448
val = quickSim black opt 10000 

--Make a call spread with a 100 unit notional
opt' = multiplier 100 
	$ vanillaOption Call 100 1 ++ short (vanillaOption Call 120 1)

 --Run a Monte Carlo on the call spread; use antithetic variates
val' = quickSimAnti black opt' 10000

black' = Black 
			100     --initial stock price
			0.2     --volatility
			(NetYC (FlatCurve 0.05) (FlatCurve 0.02))  --Now with a 2% div yield
			baseYC  --discount rate

val'' = quickSimAnti black opt' 10000

--Let's try it with a Heston model
heston = Heston
		100
		0.04    --initial variance
		0.04    --final variance
		0.2     --volvol
		-0.7    --correlation between processes
		1.0     --mean reversion speed
		baseYC  --forward generator
		baseYC  --discount function

val''' = quickSimAnti heston opt' 10000 --price the call spread in the Heston model

opt'' = terminalOnly 1 $ \x -> x*x  --create an option that pays off based on the square of its underlying

val'''' = quickSimAnti heston opt'' 10000 --price it in the Heston model

```