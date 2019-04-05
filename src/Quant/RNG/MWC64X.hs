module Quant.RNG.MWC64X (
    MWC64X(..)
  , randomWord32
  , randomWord64
  , randomInt
  , randomDouble
  , randomInt64
  , skip
) where

import Data.Int
import Data.Bits 
import Data.Word 
import Data.Random.Internal.Words
import System.Random

data MWC64X = MWC64X {-# UNPACK #-} !Word64 deriving (Eq,Show)

randomWord32 :: MWC64X -> (Word32, MWC64X)
randomWord32 (MWC64X state) = (x `xor` c, MWC64X state')
    where c = fromIntegral $ state `shiftR` 32 :: Word32 
          x = fromIntegral $ state .&. 0xFFFFFFFF :: Word32 
          state' = fromIntegral x * aConst + fromIntegral c 

randomInt :: MWC64X -> (Int,MWC64X)
randomInt g = (fromIntegral i, g')
        where (i, g') = randomWord64 g

randomWord64 :: MWC64X -> (Word64, MWC64X) 
randomWord64 x = (buildWord64'' y1 y2, x'') 
    where (y1, x' )  = randomWord32 x 
          (y2, x'') = randomWord32 x'

randomDouble :: MWC64X -> (Double, MWC64X)
randomDouble x = (fromIntegral (val `div` 2048) / 9007199254740992, x')
    where (val, x') = randomWord64 x

randomInt64 :: MWC64X -> (Int64,MWC64X)
randomInt64 g = (fromIntegral i, g')
        where (i, g') = randomWord64 g

instance RandomGen MWC64X where
    next g = (fromIntegral w, g')
        where (w, g') = randomWord64 g
    split g = (skip g skipConst, g)

addMod64 :: Word64 -> Word64 -> Word64 -> Word64
addMod64 a b m = (a+b) `mod` m

--mulMod64 :: Word64 -> Word64 -> Word64 -> Word64
--mulMod64 a b m = (a * b) `mod` m
mulMod64 :: Word64 -> Word64 -> Word64 -> Word64
mulMod64 a b m = f 0 a b
    where f r a1 b1
            | a1 == 0 = r
            | otherwise = f r' a' b'
                where r' = if a1 .&. 1 == 1 then addMod64 r b1 m else r
                      b' = addMod64 b1 b1 m
                      a' = a `shiftR` 1

powMod64 :: Word64 -> Word64 -> Word64 -> Word64
powMod64 a e m = f a 1 e
    where f sqr acc e1
            | e1 == 0 = acc
            | otherwise = f sqr' acc' e' 
                where acc' = if e1 .&. 1 == 1 then mulMod64 acc sqr m else acc
                      sqr' = mulMod64 sqr sqr m
                      e'   = e `shiftR` 1

skip :: MWC64X -> Word64 -> MWC64X
skip (MWC64X st) d = MWC64X st'
    where
        m   = powMod64 aConst d mConst
        c   = st `shiftR` 32 
        x   = st .&. 0xFFFFFFFF 
        x'  = mulMod64 (x * aConst + c) m mConst
        x'' = fromIntegral $ x' `div` aConst :: Word32
        c'  = fromIntegral $ x' `mod` aConst :: Word32
        st' = buildWord64'' c' x''

--mkWord64 :: Word32 -> Word32 -> Word64
--mkWord64 a b = (fromIntegral $ a `shiftL` 32) .&. fromIntegral b

aConst, mConst, skipConst :: Word64
aConst = 4294883355
--bConst = 4077358422479273989
mConst = 18446383549859758079
skipConst = 1073741824
