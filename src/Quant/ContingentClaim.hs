{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}


module Quant.ContingentClaim (
    -- * Types for modeling contingent claims.
    ContingentClaim
  , ContingentClaim' (..)
  , Observables (..)
  , ContingentClaimBasket (..)
  , OptionType (..)
  , ObservablePuller (..)
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
  , fixed
  , multiplier
  , short
  , combine
  , terminalOnly
  , changeObservableFct

  -- * Utility functions
  , obsNum
  , obsHead
        )  where

import Quant.MCTypes
import Data.List
import Data.Ord
import qualified Data.Vector.Unboxed as U

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
terminalOnly t f = [ContingentClaim' t head [ObservablePuller t obsHead f]]

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
    where obs = map (\x -> ObservablePuller x obsHead id) obsTimes
          f k = U.map (vanillaPayout pc strike . (/fromIntegral l))
              $ foldl1' (U.zipWith (+)) k
            where l = length k

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates a geometric Asian option.
geometricAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
geometricAsianOption pc strike obsTimes t = [ContingentClaim' t f obs]
    where obs = map (\x -> ObservablePuller x obsHead id) obsTimes
          f k = U.map (vanillaPayout pc strike . (** (1/fromIntegral l)))
              $ foldl1' (U.zipWith (*)) k
            where l = length k

-- | Scales up a contingent claim by a multiplier.
multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional cs = map f cs
    where f c@(ContingentClaim' _ collFct _) = c { collector = U.map (*notional) . collFct }

-- | Flips the signs in a contingent claim to make it a short position.
short :: ContingentClaim -> ContingentClaim
short = multiplier (-1)

-- | Takes an amount and a time and generates a fixed cash flow.
fixed :: Double -> Double -> ContingentClaim
fixed amount t = terminalOnly t $ const amount

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
ccBasket ccs = ContingentClaimBasket (sortBy (comparing (\(ContingentClaim' a _ _) -> a)) ccs) monitorTimes
    where monitorTimes = sort . nub $ concatMap (map obsTime . observations) ccs

-- | Utility function to pull the head of a basket of observables.
obsHead :: Observables a -> a
obsHead (Observables (x:_)) = x

changeObservableFct' :: ContingentClaim' -> (Observables (U.Vector Double) -> U.Vector Double) -> ContingentClaim'
changeObservableFct' c@(ContingentClaim' _ _ calcs) f = c { observations = map (\(ObservablePuller t _ g) -> ObservablePuller t f g) calcs }

-- | Offers the ability to change the function on the observable an option is based on.
--All options default to being based on the first observable.
changeObservableFct :: ContingentClaim -> (Observables (U.Vector Double) -> U.Vector Double) -> ContingentClaim
changeObservableFct ccs f = map (`changeObservableFct'` f) ccs

-- | Utility function for when the observable function is just '!!'
obsNum :: ContingentClaim -> Int -> ContingentClaim
obsNum ccs k = changeObservableFct ccs $ \(Observables x)-> x !! k