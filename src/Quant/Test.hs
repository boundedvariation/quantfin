module Quant.Test (
	baseYC
  ,	vanopt
  , vanoptPrice
  , cs
  , csPrice
  , callSpreadAnti
  , bizarre
  , bizarrePrice
  , squareOpt
  , squareOptPrice
  , heston
	)
where

import Data.Monoid
import Quant.MonteCarlo
import Quant.YieldCurve
import Quant.ContingentClaim
import Quant.Models.Black
import Quant.Models.Heston

--create a flat yield curve with a 5% rate
baseYC = FlatCurve 0.05 

black = Black 
			100     --initial stock price
			0.2     --volatility
			baseYC  --forward generator
			baseYC  --discount function

--make a vanilla put, struck at 100, maturing at time 1
vanopt = vanillaOption Call 100 1 --built in function
vanopt' = specify $ do
	x <- monitor 0 1
	return $ CashFlow 1 (max (x - 100) 0) --roll your own

--Run a Monte Carlo on opt in a a black model with 10000 trials
vanoptPrice = quickSim black vanopt 10000 

--Make a call spread with a 100 unit notional, using some handy combinators.
cs = multiplier 100 
   $ vanillaOption Call 100 1 <> short (vanillaOption Call 120 1) 

--Run a Monte Carlo on the call spread; use antithetic variates
csPrice = quickSimAnti black cs 10000 

black' = Black 
			100     --initial stock price
			0.2     --volatility
			(NetYC (FlatCurve 0.05) (FlatCurve 0.02))  --forward generator, now with a 2% dividend yield
			baseYC  --discount rate

callSpreadAnti = quickSimAnti black' cs 10000

--Let's try it with a Heston model
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
csHeston = quickSimAnti heston cs 10000 

--create an option that pays off based on the square of its underlying
squareOpt = terminalOnly 1 $ \x -> x*x  --using the built in function
squareOpt' = specify $ do --roll your own
	x <- monitor 0 1
	return $ CashFlow 1 $ x*x
squareOptPrice = quickSimAnti heston squareOpt 10000

--create an option with a bizarre payoff
bizarre = specify $ do
  x <- monitor 0 1   --check the price of asset 0 @ time 1
  y <- monitor 0 2   --check the price of asset 1 @ time 2
  z <- monitor 0 3   --check the price of asset 2 @ time 3
  return $ CashFlow 4 $ x ^ 3 / y ^ 2 - 3 * z --payoff @ time 4
bizarrePrice = quickSimAnti heston bizarre 10000 

