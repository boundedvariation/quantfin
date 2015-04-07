module Quant.Math.Utilities (
    tdmaSolver
  --, cubicSpline
) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

tdmaSolver :: (Fractional a, Ord a) => [a] -> [a] -> [a] -> [a] -> [a]
tdmaSolver aL bL cL dL = V.toList $ 
    let [a,b,c,d] = map V.fromList [aL,bL,cL,dL] in 
        runST $ do
            c' <- V.thaw c
            M.write c' 0 (V.head c / V.head b)
            forM_ [1..V.length c-1] $ \x -> do
                let ai = a V.! x
                    bi = b V.! x
                    ci = c V.! x
                ci1' <- M.read c' (x-1)
                M.write c' x $ ci / (bi-ai*ci1')
            cf <- V.unsafeFreeze c'
            d' <- V.thaw d
            M.write d' 0 (V.head d / V.head b)
            forM_ [1..V.length d-1] $ \x -> do
                let ai  = a  V.! x
                    bi  = b  V.! x
                    di  = d  V.! x
                    ci1 = cf V.! (x-1)
                di1' <- M.read d' (x-1)
                M.write d' x $ (di-ai*di1') / (bi-ai*ci1)
            df <- V.unsafeFreeze d'
            xn <- M.new $ V.length d
            M.write xn (V.length d-1) $ V.last df
            forM_ (reverse [0..V.length df-2]) $ \ x-> do
                let ci = cf V.! x
                    di = df V.! x
                xi1 <- M.read xn $ x+1
                M.write xn x $ di - ci*xi1
            V.unsafeFreeze xn