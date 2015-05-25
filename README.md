# quantfin
Quant Finance in Pure Haskell.

Initially I'm focusing on a Monte Carlo engine, but plenty more to come.

```haskell


import Quant.Time
import Data.Monoid
import Quant.MonteCarlo
import Quant.YieldCurve
import Quant.ContingentClaim
import Quant.Models.Black
import Quant.Models.Heston

--create a flat yield curve with a 5% rate
baseYC :: FlatCurve
baseYC = FlatCurve 0.05 

black :: Black
black = Black 
			100     --initial stock price
			0.2     --volatility
			baseYC  --forward generator
			baseYC  --discount function

--make a vanilla put, struck at 100, maturing at time 1
vanopt :: CC1
vanopt = vanillaOption Call 100 (Time 1) --built in function

vanopt' :: CC1
vanopt' = specify $ do
	x <- monitor (Time 1)
	return $ CashFlow (Time 1) (max (x - 100) 0) --roll your own

--Run a Monte Carlo on opt in a a black model with 10000 trials
vanoptPrice :: Double
vanoptPrice = quickSim black vanopt 100000 

vanoptPrice' :: Double
vanoptPrice' = quickSim black vanopt' 100000 


--Make a call spread with a 100 unit notional, using some handy combinators.
cs :: CC1
cs =  multiplier 100 
   $  vanillaOption Call 100 (Time 1) 
   <> short (vanillaOption Call 120 (Time 1)) 

--Run a Monte Carlo on the call spread; use antithetic variates
csPrice :: Double
csPrice = quickSim black' cs 100000 

black' :: Black
black' = Black 
			100     --initial stock price
			0.2     --volatility
			(NetYC (FlatCurve 0.05) (FlatCurve 0.02))  --forward generator, now with a 2% dividend yield
			baseYC  --discount rate

callSpreadAnti :: Double
callSpreadAnti = quickSimAnti black' cs 100000

--Let's try it with a Heston model
heston :: Heston
heston = Heston
		100
		0.04       --initial variance
		0.04       --final variance
		0.2        --volvol
		(-0.7)     --correlation between processes
		1.0        --mean reversion speed
		baseYC     --forward generator
		baseYC     --discount function

--price the call spread in the Heston model
csHeston :: Double
csHeston = quickSimAnti heston cs 100000

--create an option that pays off based on the square of its underlying
squareOpt :: CC1
squareOpt = terminalOnly (Time 1) $ \x -> x*x  --using the built in function

squareOpt' :: CC1
squareOpt' = specify $ do --roll your own
	x <- monitor (Time 1)
	return $ CashFlow (Time 1) $ x*x
squareOptPrice :: Double
squareOptPrice = quickSimAnti black squareOpt 100000

squareOptPrice' :: Double
squareOptPrice' = quickSimAnti black squareOpt' 100000


--create an option with a bizarre payoff
bizarre :: CC1
bizarre = specify $ do
  x <- monitor (Time 1)   --check the price of asset 0 @ time 1
  y <- monitor (Time 2)   --check the price of asset 0 @ time 2
  z <- monitor (Time 3)   --check the price of asset 0 @ time 3
  return $ CashFlow (Time 4) $ sin x * cos y / (z ** sin x) --payoff @ time 4
bizarrePrice :: Double
bizarrePrice = quickSimAnti black bizarre 100000

```