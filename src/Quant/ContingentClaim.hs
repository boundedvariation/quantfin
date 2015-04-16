module Quant.ContingentClaim (
    -- * Types for modeling contingent claims.
    ContingentClaim
  , ContingentClaim' (..)
  , ObservablePuller (..)
  , Observables (..)
  , MCObservables
  , ContingentClaimBasket (..)
  , OptionType (..)
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
  , changeObservableFct

  -- * Utility functions
  , obsNum
)  where

import Data.List
import Data.Ord


-- | 'ContingentClaim'' is the underlying type of contingent claims.
data ContingentClaim' = ContingentClaim' {
    payoutTime   :: Double                               -- ^ Payout time for cash flow
  , collector    :: [Double]  -> Double 
  , observations :: [ObservablePuller]
}

data ObservablePuller = ObservablePuller {
    obsTime      :: Double
  , obsGetter    :: MCObservables -> Double
}

-- | 'ContingentClaim' is just a list of the underlying 'ContingentClaim''s.
type ContingentClaim = [ContingentClaim']

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
terminalOnly t f = [ContingentClaim' t (f . head) [ObservablePuller t (head . obsGet)]]

-- | Takes an OptionType, a strike, and a time to maturity and generates a vanilla option.
vanillaOption :: OptionType -> Double -> Double -> ContingentClaim
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

-- | Takes an OptionType, a strike, a payout amount and a time to 
--maturity and generates a vanilla option.
binaryOption :: OptionType -> Double -> Double -> Double -> ContingentClaim
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
arithmeticAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
arithmeticAsianOption pc strike obsTimes t = [ContingentClaim' t f obs]
    where obs = map (\x -> ObservablePuller x (head . obsGet)) obsTimes
          f k = vanillaPayout pc strike . (/fromIntegral l)
              $ foldl1' (+) k
            where l = length k

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates a geometric Asian option.
geometricAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
geometricAsianOption pc strike obsTimes t = [ContingentClaim' t f obs]
    where obs = map (\x -> ObservablePuller x (head . obsGet)) obsTimes
          f k = vanillaPayout pc strike . (** (1/fromIntegral l))
              $ foldl1' (*) k
            where l = length k

-- | Scales up a contingent claim by a multiplier.
multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional cs = map f cs
    where f c@(ContingentClaim' _ collFct _) = c { collector = (*notional) . collFct }

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
data ContingentClaimBasket = ContingentClaimBasket ContingentClaim [Double]

-- | Converts a 'ContingentClaim' into a 'ContingentClaimBasket' for use by the MC engine.
ccBasket :: ContingentClaim -> ContingentClaimBasket
ccBasket ccs = ContingentClaimBasket (sortBy (comparing payoutTime) ccs) monitorTimes
    where monitorTimes = sort . nub $ concatMap (map obsTime . observations) ccs

changeObservableFct' :: ContingentClaim' -> (MCObservables -> Double) -> ContingentClaim'
changeObservableFct' c@(ContingentClaim' _ _ calcs) f = c { observations = map (\(ObservablePuller t _) -> ObservablePuller t f) calcs }

-- | Offers the ability to change the function on the observable an option is based on.
--All options default to being based on the first observable.
changeObservableFct :: ContingentClaim -> (MCObservables -> Double) -> ContingentClaim
changeObservableFct ccs f = map (`changeObservableFct'` f) ccs

-- | Utility function for when the observable function is just '!!'
obsNum :: ContingentClaim -> Int -> ContingentClaim
obsNum ccs k = changeObservableFct ccs $ (!!k) . obsGet