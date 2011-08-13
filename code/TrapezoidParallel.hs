module TrapezoidParallel (trapezoid, f) where

import GHC.Conc
import Control.Parallel.Strategies
import Data.List

trapezoid :: (Double -> Double)  -- Function to integrate
             -> Double -> Double -- integration bounds
             -> Int              -- number trapezoids
             -> Double           -- width of a trapezoid
             -> Double
trapezoid f a b n h =
  h * (endPoints + internals)
  where
    sum' = foldl' (+) 0
    endPoints   = (f a + f b) / 2
    internals   = sum' ( [ sum' (map f c) | c <- chunks ] {-`using` parList rdeepseq-} )
    numChunks   = numCapabilities * 32
    chunkLength = n `div` numChunks
    chunks      = splitInto chunkLength $ take (n-1) $ iterate (+h) (a + h)
    
splitInto len = unfoldr next
  where
    next []  = Nothing
    next lst = Just (splitAt len lst)

f :: Double -> Double
f x = 4 / (1 + x * x)
