module Quant.ContingentClaim (
    ContingentClaim
  , ContingentClaim' (..)
  , Observables (..)
  , ContingentClaimBasket (..)
  , OptionType (..)
  , ccBasket
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
  , obsNum
  , obsHead
        )  where

import Data.List
import Data.Ord
import qualified Data.Vector.Unboxed as U

data ContingentClaim' = ContingentClaim' {
    payoutTime   :: Double
  , collector    :: [U.Vector Double] -> U.Vector Double
  , observations :: [( Double                          --time of observation
                     , Observables  -> U.Vector Double --Observables puller
                     , Double       -> Double) ]       --Function to run
}

type ContingentClaim = [ContingentClaim']

data Observables = Observables [U.Vector Double] deriving (Eq, Show)

data OptionType = Put | Call deriving (Eq,Show)

vanillaPayout :: OptionType -> Double -> Double -> Double
vanillaPayout pc strike x = case pc of
    Put  -> max (strike - x) 0
    Call -> max (x - strike) 0

binaryPayout :: OptionType -> Double -> Double -> Double -> Double
binaryPayout pc strike amount x = case pc of
    Put  -> if strike > x then amount else 0
    Call -> if x > strike then amount else 0

terminalOnly :: Double -> (Double -> Double) -> ContingentClaim
terminalOnly t f = [ContingentClaim' t head [(t, obsHead, f)]]

vanillaOption :: OptionType -> Double -> Double -> ContingentClaim
vanillaOption pc strike t = terminalOnly t $ vanillaPayout pc strike

binaryOption :: OptionType -> Double -> Double -> Double -> ContingentClaim
binaryOption pc strike amount t = terminalOnly t $ binaryPayout pc strike amount

arithmeticAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
arithmeticAsianOption pc strike obsTimes t = [ContingentClaim' t f obs]
    where obs = map (\x -> (x, obsHead, id)) obsTimes
          f k = U.map (vanillaPayout pc strike . (/fromIntegral l))
              $ foldl1' (U.zipWith (+)) k
            where l = length k

geometricAsianOption :: OptionType -> Double -> [Double] -> Double -> ContingentClaim
geometricAsianOption pc strike obsTimes t = [ContingentClaim' t f obs]
    where obs = map (\x -> (x, obsHead, id)) obsTimes
          f k = U.map (vanillaPayout pc strike . (** (1/fromIntegral l)))
              $ foldl1' (U.zipWith (*)) k
            where l = length k

multiplier :: Double -> ContingentClaim -> ContingentClaim
multiplier notional cs = map f cs
    where f c@(ContingentClaim' _ collFct _) = c { collector = U.map (*notional) . collFct }

short :: ContingentClaim -> ContingentClaim
short = multiplier (-1)

fixed :: Double -> Double -> ContingentClaim
fixed amount t = terminalOnly t $ const amount

forwardContract :: Double -> ContingentClaim
forwardContract t = terminalOnly t id

callSpread :: Double -> Double -> Double -> ContingentClaim
callSpread lowStrike highStrike t = combine (vanillaOption Call lowStrike t) (short $ vanillaOption Call highStrike t)

putSpread :: Double -> Double -> Double -> ContingentClaim
putSpread lowStrike highStrike t = combine (vanillaOption Put highStrike t) (short $ vanillaOption Put lowStrike t)

straddle :: Double -> Double -> ContingentClaim
straddle strike t = vanillaOption Put strike t ++ vanillaOption Call strike t

combine :: ContingentClaim -> ContingentClaim -> ContingentClaim
combine = (++)

data ContingentClaimBasket = ContingentClaimBasket ContingentClaim [Double]

ccBasket :: ContingentClaim -> ContingentClaimBasket
ccBasket ccs = ContingentClaimBasket (sortBy (comparing payoutTime) ccs) monitorTimes
    where monitorTimes = sort . nub $ concatMap (map fst3 . observations) ccs

obsHead :: Observables -> U.Vector Double
obsHead (Observables (x:_)) = x

changeObservableFct' :: ContingentClaim' -> (Observables -> U.Vector Double) -> ContingentClaim'
changeObservableFct' c@(ContingentClaim' _ _ calcs) f = c { observations = map (\(t, _, g) -> (t, f, g)) calcs }

changeObservableFct :: ContingentClaim -> (Observables -> U.Vector Double) -> ContingentClaim
changeObservableFct ccs f = map (`changeObservableFct'` f) ccs

fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

obsNum :: ContingentClaim -> Int -> ContingentClaim
obsNum ccs k = changeObservableFct ccs ((!! k) . ( \(Observables x)-> x))