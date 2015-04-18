module Quant.ContingentClaim (
    -- * Types for modeling contingent claims.
    ContingentClaim
  , CCProcessor (..)
  , Observables (..)
  , MCObservables
  , ContingentClaimBasket (..)
  , OptionType (..)
  , CashFlow (..)
  , ccBasket

  -- * Options and option combinators
  , vanillaOption
  , binaryOption
  , straddle
  , arithmeticAsianOption
  , geometricAsianOption
  , callSpread
  , putSpread
  , forwardContract
  , zcb
  , fixedBond
  , multiplier
  , short
  , combine
  , terminalOnly
  , changeObservableNum

)  where

import Data.List
import Data.Ord
import Quant.Types
import qualified Data.Map as M

data CCProcessor = Monitor  {
                      monitorTimes  :: TimeOffset
                    , observableNum :: Int
                  }
                 | Payout   {
                      evalTime     :: TimeOffset
                    , payoutFunc   :: (M.Map TimeOffset MCObservables -> (CashFlow, Continue))
                  }

data CashFlow = CashFlow {
    cfTime   :: TimeOffset
  , cfAmount :: Double
}

data Continue = Terminate | Proceed

type ContingentClaim = [CCProcessor]

-- | Observables are the observables available in a Monte Carlo simulation.
--Most basic MCs will have one observables (Black-Scholes) whereas more
--complex ones will have multiple (i.e. Heston-Hull-White).
data Observables a = Observables { obsGet :: [a] } deriving (Eq, Show)

type MCObservables = Observables Double

-- | Type for Put or Calls
data OptionType = Put | Call deriving (Eq,Show)

-- | Function to generate a vanilla put/call style payout.
vanillaPayout :: OptionType  -- ^ Put or Call
              -> Double      -- ^ Strike
              -> Double      -- ^ Observable val
              -> Double      -- ^ Price
vanillaPayout pc strike x = case pc of
    Put  -> max (strike - x) 0
    Call -> max (x - strike) 0

-- | Function to generate a binary option payout.
binaryPayout :: OptionType  -- ^ Put or call
             -> Double      -- ^ strike
             -> Double      -- ^ Payout amount if binary condition achieved
             -> Double      -- ^ observable level
             -> Double      -- ^ calculated payout
binaryPayout pc strike amount x = case pc of
    Put  -> if strike > x then amount else 0
    Call -> if x > strike then amount else 0

-- | Takes a maturity time and a function and generates a ContingentClaim 
--dependent only on the terminal value of the observable.
terminalOnly :: Double -> (Double -> Double) -> ContingentClaim
terminalOnly t g = [Monitor t 0
                 ,  Payout  t f    ]
                  where f m = (CashFlow t (g val), Terminate)
                            where val = head . obsGet $ m M.! t

-- | Takes an OptionType, a strike, and a time to maturity and generates a vanilla option.
vanillaOption :: OptionType -> Double -> Double -> ContingentClaim
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

-- | Takes an OptionType, a strike, a payout amount and a time to 
--maturity and generates a vanilla option.
binaryOption :: OptionType -> Double -> Double -> Double -> ContingentClaim
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
arithmeticAsianOption :: OptionType -> Double -> [TimeOffset] -> TimeOffset -> ContingentClaim
arithmeticAsianOption pc strike obsTimes t = monTimesList obsTimes 0 ++ [Payout t f]
    where f k = (CashFlow t cf, Terminate)
                where
                  cf =  vanillaPayout pc strike . (/ fromIntegral l)
                      $ foldl1' (+) j
                  j = map (head . obsGet . (\j -> k M.! j)) obsTimes
                  l = length j

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
geometricAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
geometricAsianOption pc strike obsTimes t = monTimesList obsTimes 0 ++ [Payout t f]
    where f k = (CashFlow t cf, Terminate)
                where
                  cf =  vanillaPayout pc strike . (** (1/fromIntegral l))
                      $ foldl1' (*) j
                  j = map (head . obsGet . (\j -> k M.! j)) obsTimes
                  l = length j

-- | Scales up a contingent claim by a multiplier.
multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional cs = map f cs
    where f (Payout t g) = Payout t l
            where l x = (CashFlow t (p * notional), cont)
                    where (CashFlow t p, cont) = g x
          f k = k

-- | Flips the signs in a contingent claim to make it a short position.
short :: ContingentClaim -> ContingentClaim
short = multiplier (-1)

-- | Takes an amount and a time and generates a fixed cash flow.
zcb :: Double -> Double -> ContingentClaim
zcb amount t = terminalOnly t $ const amount

-- | Takes a face value, an interest rate, a payment frequency and makes a fixed bond
fixedBond :: Double -> Double -> Double -> Int -> ContingentClaim
fixedBond faceVal intRate freq pmts = zcb faceVal (fromIntegral pmts * freq) ++ concatMap f [1..pmts]
  where
    f = zcb (faceVal * intRate * freq) . fromIntegral 

-- | Takes a time to maturity and generates a forward contract.
forwardContract :: Double -> ContingentClaim
forwardContract t = terminalOnly t id

-- | A call spread is a long position in a low-strike call
--and a short position in a high strike call.
callSpread :: Double -> Double -> Double -> ContingentClaim
callSpread lowStrike highStrike t = combine (vanillaOption Call lowStrike t) (short $ vanillaOption Call highStrike t)

-- | A put spread is a long position in a high strike put
--and a short position in a low strike put.
putSpread :: Double -> Double -> Double -> ContingentClaim
putSpread lowStrike highStrike t = combine (vanillaOption Put highStrike t) (short $ vanillaOption Put lowStrike t)

-- | A straddle is a put and a call with the same time to maturity / strike.
straddle :: Double -> Double -> ContingentClaim
straddle strike t = vanillaOption Put strike t ++ vanillaOption Call strike t

-- | Just combines two contingent claims into one. 
combine :: ContingentClaim -> ContingentClaim -> ContingentClaim
combine = (++)

-- | Used to compile claims for the Monte Carlo engine.
data ContingentClaimBasket = ContingentClaimBasket {
    ccObsTimes     :: [TimeOffset]
  , ccPayoutTimes  :: [TimeOffset]
  , strippedCCs    :: [ContingentClaim]
}

-- | Converts a 'ContingentClaim' into a 'ContingentClaimBasket' for use by the MC engine.
ccBasket :: [ContingentClaim] -> ContingentClaimBasket
ccBasket ccs = ContingentClaimBasket mTimes pTimes ccs'
    where 
      mTimes = sort . nub . map getTime . filter monitor $ concat ccs
      pTimes = sort . nub . map getTime . filter (not . monitor) $ concat ccs
      ccs' = map (filter (not . monitor)) ccs

changeObservableNum :: ContingentClaim -> Int -> ContingentClaim
changeObservableNum xs i = map f xs
  where f (Monitor t _) = Monitor t i
        f j = j

monTimesList :: [Double] -> Int -> [CCProcessor]
monTimesList ts i = map (\t -> Monitor t i) ts

getTime :: CCProcessor -> Double
getTime (Monitor t _) = t
getTime (Payout t _) = t

monitor :: CCProcessor -> Bool
monitor (Monitor _ _) = True
monitor _ = False