module Quant.ContingentClaim (
    -- * Types for modeling contingent claims.
    ContingentClaim (..)
  , CCProcessor (..)
  , Observables (..)
  , MCObservables
  , OptionType (..)
  , CashFlow (..)
  , CCBuilder

  -- * Options and option combinators
  , specify
  , monitor
  , monitorByNum
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

)  where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Quant.Types
import Quant.Time
import qualified Data.Map as M

type MCMap = M.Map Time MCObservables
type PayoffFunc a = MCMap -> a


data CCProcessor = CCProcessor  {
                      monitorTime      :: Time
                    , payoutFunc       :: Maybe [PayoffFunc CashFlow]
}


type CCBuilder w r a = WriterT w (Reader r) a

monitor :: Time -> CCBuilder ContingentClaim MCMap Double
monitor = monitorByNum 0

monitorByNum :: Int -> Time -> CCBuilder ContingentClaim MCMap Double
monitorByNum idx t = do
  tell $ ContingentClaim [CCProcessor t Nothing]
  m <- lift ask
  return $ obsGet (m M.! t) !! idx  --I know, I know.

specify :: CCBuilder ContingentClaim MCMap CashFlow -> ContingentClaim
specify x = w `mappend` ContingentClaim [CCProcessor (last0 w') (Just [f])]
  where
    w  = runReader (execWriterT x) M.empty
    f  = runReader . liftM fst $ runWriterT x
    w' = map monitorTime $ unCC w
    -- Equivalent to Prelude's last, but with a default of zero
    last0 [] = Time 0
    last0 [y] = y
    last0 (_:ys) = last0 ys

newtype ContingentClaim = ContingentClaim { unCC :: [CCProcessor] }

instance Monoid ContingentClaim where
  mempty  = ContingentClaim []
  mappend = combine

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
terminalOnly :: Time -> (Double -> Double) -> ContingentClaim
terminalOnly t g = specify $ do
  x <- monitor t
  return $ CashFlow t $ g x

-- | Takes an OptionType, a strike, and a time to maturity and generates a vanilla option.
vanillaOption :: OptionType -> Double -> Time -> ContingentClaim
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

-- | Takes an OptionType, a strike, a payout amount and a time to 
--maturity and generates a vanilla option.
binaryOption :: OptionType -> Double -> Double -> Time -> ContingentClaim
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
arithmeticAsianOption :: OptionType -> Double -> [Time] -> Time -> ContingentClaim
arithmeticAsianOption pc strike obsTimes t = specify $ do
  x <- mapM monitor obsTimes
  let avg = sum x / fromIntegral (length obsTimes)
  return $ CashFlow t $ vanillaPayout pc strike avg

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
geometricAsianOption :: OptionType -> Double -> [Time] -> Time -> ContingentClaim
geometricAsianOption pc strike obsTimes t = specify $ do
  x <- mapM monitor obsTimes
  let avg = product x ** (1 / fromIntegral (length obsTimes))
  return $ CashFlow t $ vanillaPayout pc strike avg

-- | Scales up a contingent claim by a multiplier.
multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional cs = ContingentClaim $ map f (unCC cs)
    where f (CCProcessor t g) = CCProcessor t $ fmap (fmap (scale.)) g
          scale (CashFlow dt amt) = CashFlow dt (amt*notional)

-- | Flips the signs in a contingent claim to make it a short position.
short :: ContingentClaim -> ContingentClaim
short = multiplier (-1)

-- | Takes an amount and a time and generates a fixed cash flow.
zcb :: Time -> Double -> ContingentClaim
zcb t amt = specify $ return $ CashFlow t amt

-- | Takes a face value, an interest rate, a payment frequency and makes a fixed bond
fixedBond :: Double -> Double -> Double -> Int -> ContingentClaim
fixedBond faceVal intRate freq pmts = zcb (Time $ fromIntegral pmts * freq) faceVal 
                                   <> mconcat (map f [1..pmts])
  where
    f x = zcb (Time $ fromIntegral x * freq) (faceVal * intRate * freq) 

-- | Takes a time to maturity and generates a forward contract.
forwardContract :: Time -> ContingentClaim
forwardContract t = specify $ do
  x <- monitor t
  return $ CashFlow t x

-- | A call spread is a long position in a low-strike call
--and a short position in a high strike call.
callSpread :: Double -> Double -> Time -> ContingentClaim
callSpread lowStrike highStrike t = mappend (vanillaOption Call lowStrike t) 
                                            (short $ vanillaOption Call highStrike t)

-- | A put spread is a long position in a high strike put
--and a short position in a low strike put.
putSpread :: Double -> Double -> Time -> ContingentClaim
putSpread lowStrike highStrike t = mappend (vanillaOption Put highStrike t) 
                                           (short $ vanillaOption Put lowStrike t)

-- | A straddle is a put and a call with the same time to maturity / strike.
straddle :: Double -> Time -> ContingentClaim
straddle strike t = vanillaOption Put strike t <> vanillaOption Call strike t

-- | Combines two contingent claims into one. 
combine :: ContingentClaim -> ContingentClaim -> ContingentClaim
combine (ContingentClaim x) (ContingentClaim y) = ContingentClaim $ combine' x y
  where
    combine' (cc1:ccs1) (cc2:ccs2)
      | monitorTime cc1 == monitorTime cc2 = let
          (CCProcessor t mf)  = cc1
          (CCProcessor _ mf') = cc2 in
            case mf of
              Nothing -> cc2 : combine' ccs1 ccs2
              Just a  -> case mf' of
                Nothing -> cc1 : combine' ccs1 ccs2
                Just b  -> CCProcessor t (Just (a ++ b)) : combine' ccs1 ccs2
      | monitorTime cc1 > monitorTime cc2 = cc2 : combine' (cc1:ccs1) ccs2
      | otherwise = cc1 : combine' ccs1 (cc2:ccs2)
    combine' [] [] = []
    combine' cs [] = cs
    combine' [] cs = cs