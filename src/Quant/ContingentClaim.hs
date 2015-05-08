module Quant.ContingentClaim (
    -- * Types for modeling contingent claims.
    ContingentClaim (..)
  , CCProcessor (..)
  , Observables (..)
  , MCObservables
  , OptionType (..)
  , CashFlow (..)

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
  , getTime

)  where

import Data.List
import Data.Monoid
import Quant.Types
import qualified Data.Map as M

data CCProcessor = Monitor  {
                      monitorTime      :: TimeOffset
                  }
                 | Payout   {
                      evalTime         :: TimeOffset
                    , payoutFunc       :: M.Map TimeOffset MCObservables -> CashFlow
                  }

data CashFlow = CashFlow {
    cfTime   :: TimeOffset
  , cfAmount :: Double
}

newtype ContingentClaim = ContingentClaim { unCC :: [CCProcessor] }

instance Monoid ContingentClaim where
  mempty  = ContingentClaim []
  mappend = combine

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
terminalOnly t g = ContingentClaim [Monitor t
                                 ,  Payout  t f  ]
                  where f m = CashFlow t (g val)
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
arithmeticAsianOption pc strike obsTimes t = ContingentClaim $ monTimesList obsTimes ++ [Payout t f ]
    where f k = CashFlow t cf
                where
                  cf =  vanillaPayout pc strike . (/ fromIntegral l)
                      $ foldl1' (+) j
                  j = map (head . obsGet . (\idx -> k M.! idx)) obsTimes
                  l = length j

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
geometricAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
geometricAsianOption pc strike obsTimes t = ContingentClaim $ monTimesList obsTimes ++ [Payout t f ]
    where f k = CashFlow t cf
                where
                  cf =  vanillaPayout pc strike . (** (1/fromIntegral l))
                      $ foldl1' (*) j
                  j = map (head . obsGet . (\idx -> k M.! idx)) obsTimes
                  l = length j

-- | Scales up a contingent claim by a multiplier.
multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional cs = ContingentClaim $ map f (unCC cs)
    where f (Payout t g) = Payout t l
            where l x = CashFlow t' (p * notional)
                    where CashFlow t' p = g x
          f k = k

-- | Flips the signs in a contingent claim to make it a short position.
short :: ContingentClaim -> ContingentClaim
short = multiplier (-1)

-- | Takes an amount and a time and generates a fixed cash flow.
zcb :: Double -> Double -> ContingentClaim
zcb amount t = terminalOnly t $ const amount

-- | Takes a face value, an interest rate, a payment frequency and makes a fixed bond
fixedBond :: Double -> Double -> Double -> Int -> ContingentClaim
fixedBond faceVal intRate freq pmts = zcb faceVal (fromIntegral pmts * freq) 
                                   <> (mconcat $ map f [1..pmts])
  where
    f = zcb (faceVal * intRate * freq) . fromIntegral 

-- | Takes a time to maturity and generates a forward contract.
forwardContract :: Double -> ContingentClaim
forwardContract t = terminalOnly t id

-- | A call spread is a long position in a low-strike call
--and a short position in a high strike call.
callSpread :: Double -> Double -> Double -> ContingentClaim
callSpread lowStrike highStrike t = mappend (vanillaOption Call lowStrike t) (short $ vanillaOption Call highStrike t)

-- | A put spread is a long position in a high strike put
--and a short position in a low strike put.
putSpread :: Double -> Double -> Double -> ContingentClaim
putSpread lowStrike highStrike t = mappend (vanillaOption Put highStrike t) (short $ vanillaOption Put lowStrike t)

-- | A straddle is a put and a call with the same time to maturity / strike.
straddle :: Double -> Double -> ContingentClaim
straddle strike t = vanillaOption Put strike t <> vanillaOption Call strike t

-- | Just combines two contingent claims into one. 
combine :: ContingentClaim -> ContingentClaim -> ContingentClaim
combine (ContingentClaim x) (ContingentClaim y) = ContingentClaim $ combine' x y
  where
    combine' (cc1:ccs1) (cc2:ccs2)
      | getTime cc1 >= getTime cc2 = cc2 : combine' (cc1:ccs1) ccs2
      | otherwise = cc1 : combine' ccs1 (cc2:ccs2)
    combine' [] [] = []
    combine' cs [] = cs
    combine' [] cs = cs

monTimesList :: [Double] -> [CCProcessor]
monTimesList ts = map (\t -> Monitor t) ts

getTime :: CCProcessor -> Double
getTime (Monitor t) = t
getTime (Payout t _) = t