{-# LANGUAGE RankNTypes #-}

module Quant.ContingentClaim (
    -- * Types for modeling contingent claims.
    ContingentClaim (..)
  , ContingentClaim1
  , ContingentClaim2
  , ContingentClaim3
  , ContingentClaim4
  , CCProcessor (..)
  , OptionType (..)
  , CashFlow (..)
  , CCBuilder

  -- * Options and option combinators
  , specify
  , monitor
  , monitor1
  , monitor2
  , monitor3
  , monitor4
  , monitor5
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

-- |Key type for building contingent claims.
--Monoid instance allows for trivial combinations of
--contingent claims.
newtype ContingentClaim a = ContingentClaim { unCC :: [CCProcessor a] }

-- |Contingent claims with one observable.
type ContingentClaim1 = forall a . Obs1 a => ContingentClaim a
-- |Contingent claims with two observables.
type ContingentClaim2 = forall a . Obs2 a => ContingentClaim a
-- |Contingent claims with three observables.
type ContingentClaim3 = forall a . Obs3 a => ContingentClaim a
-- |Contingent claims with four observables.
type ContingentClaim4 = forall a . Obs4 a => ContingentClaim a

instance Monoid (ContingentClaim a) where
  mempty  = ContingentClaim []
  mappend = combine

-- |Basic element of a `ContingentClaim`.  Each element contains
--a Time.  Each Time, the observables are stored in the map.
--Also, optionally a payout function may be applied at any time step.
data CCProcessor a = CCProcessor  {
                      monitorTime      :: Time
                    , payoutFunc       :: Maybe [M.Map Time a -> CashFlow]
}

type CCBuilder w r a = WriterT w (Reader r) a

monitor :: Obs1 a => Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitor = monitor1

monitor1 :: Obs1 a => Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitor1 = monitorGeneric get1

monitor2 :: Obs2 a => Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitor2 = monitorGeneric get2

monitor3 :: Obs3 a => Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitor3 = monitorGeneric get3

monitor4 :: Obs4 a => Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitor4 = monitorGeneric get4

monitor5 :: Obs5 a => Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitor5 = monitorGeneric get5

monitorGeneric :: (a -> Double) -> Time -> CCBuilder (ContingentClaim a) (M.Map Time a) Double
monitorGeneric f t = do
  tell $ ContingentClaim [CCProcessor t Nothing]
  m <- lift ask
  return $ f (m M.! t)

-- |Pulls a ContingentClaim out of the CCBuilder monad.
specify :: CCBuilder (ContingentClaim a) (M.Map Time a) CashFlow -> ContingentClaim a
specify x = w `mappend` ContingentClaim [CCProcessor (last0 w') (Just [f])]
  where
    w  = runReader (execWriterT x) M.empty
    f  = runReader . liftM fst $ runWriterT x
    w' = map monitorTime $ unCC w
    -- Equivalent to Prelude's last, but with a default of zero
    last0 [] = Time 0
    last0 [y] = y
    last0 (_:ys) = last0 ys


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
terminalOnly :: Obs1 a => Time -> (Double -> Double) -> ContingentClaim a
terminalOnly t g = specify $ do
  x <- monitor t
  return $ CashFlow t $ g x

-- | Takes an OptionType, a strike, and a time to maturity and generates a vanilla option.
vanillaOption :: Obs1 a => OptionType -> Double -> Time -> ContingentClaim a
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

-- | Takes an OptionType, a strike, a payout amount and a time to 
--maturity and generates a vanilla option.
binaryOption :: Obs1 a => OptionType -> Double -> Double -> Time -> ContingentClaim a
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
arithmeticAsianOption :: Obs1 a => OptionType -> Double -> [Time] -> Time -> ContingentClaim a
arithmeticAsianOption pc strike obsTimes t = specify $ do
  x <- mapM monitor obsTimes
  let avg = sum x / fromIntegral (length obsTimes)
  return $ CashFlow t $ vanillaPayout pc strike avg

-- | Takes an OptionType, a strike, observation times, time to
--maturity and generates an arithmetic Asian option.
geometricAsianOption :: Obs1 a => OptionType -> Double -> [Time] -> Time -> ContingentClaim a
geometricAsianOption pc strike obsTimes t = specify $ do
  x <- mapM monitor obsTimes
  let avg = product x ** (1 / fromIntegral (length obsTimes))
  return $ CashFlow t $ vanillaPayout pc strike avg

-- | Scales up a contingent claim by a multiplier.
multiplier :: Double -> ContingentClaim a -> ContingentClaim a
multiplier notional cs = ContingentClaim $ map f (unCC cs)
    where f (CCProcessor t g) = CCProcessor t $ fmap (fmap (scale.)) g
          scale (CashFlow dt amt) = CashFlow dt (amt*notional)

-- | Flips the signs in a contingent claim to make it a short position.
short :: ContingentClaim a -> ContingentClaim a
short = multiplier (-1)

-- | Takes an amount and a time and generates a fixed cash flow.
zcb :: Time -> Double -> ContingentClaim a
zcb t amt = specify $ return $ CashFlow t amt

-- | Takes a face value, an interest rate, a payment frequency and makes a fixed bond
fixedBond :: Double -> Double -> Double -> Int -> ContingentClaim a
fixedBond faceVal intRate freq pmts = zcb (Time $ fromIntegral pmts * freq) faceVal 
                                   <> mconcat (map f [1..pmts])
  where
    f x = zcb (Time $ fromIntegral x * freq) (faceVal * intRate * freq) 

-- | Takes a time to maturity and generates a forward contract.
forwardContract :: Obs1 a => Time -> ContingentClaim a
forwardContract t = specify $ do
  x <- monitor t
  return $ CashFlow t x

-- | A call spread is a long position in a low-strike call
--and a short position in a high strike call.
callSpread :: Obs1 a => Double -> Double -> Time -> ContingentClaim a
callSpread lowStrike highStrike t = mappend (vanillaOption Call lowStrike t) 
                                            (short $ vanillaOption Call highStrike t)

-- | A put spread is a long position in a high strike put
--and a short position in a low strike put.
putSpread :: Obs1 a => Double -> Double -> Time -> ContingentClaim a
putSpread lowStrike highStrike t = mappend (vanillaOption Put highStrike t) 
                                           (short $ vanillaOption Put lowStrike t)

-- | A straddle is a put and a call with the same time to maturity / strike.
straddle :: Obs1 a => Double -> Time -> ContingentClaim a
straddle strike t = vanillaOption Put strike t <> vanillaOption Call strike t

-- | Combines two contingent claims into one. 
combine :: ContingentClaim a -> ContingentClaim a -> ContingentClaim a
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