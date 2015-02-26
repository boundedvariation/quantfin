{-# LANGUAGE ExistentialQuantification #-}

module Quant.Models.Black (
    Black (..)
) where

import Quant.RNProcess
import Quant.Models
import Data.Complex

{- | 'Black' represents a Black-Scholes
model with a yield curve for a 
-}
data Black = forall a . ForwardGen a => Black Double Double a

instance CharFunc Black where
    charFunc (Black _ vol _) t k = exp 
        $ negate i*vol'*vol'/2.0*t'*k-vol'*vol'*k*k/2.0*t'
        where
            i = 0 :+ 1
            t' = t :+ 0
            vol' = vol :+ 0

