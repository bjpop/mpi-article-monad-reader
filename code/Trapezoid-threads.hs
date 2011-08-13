module Main where

import Control.Concurrent
import GHC.Conc
import Control.Parallel.Strategies
import System (getArgs)
import Data.List

main :: IO ()
main = do
  let maxThreads = numCapabilities
  aStr:bStr:nStr:_ <- getArgs
  let [a,b] = map read [aStr,bStr]
      n = read nStr
      h = (b - a) / fromIntegral n
      localN = n `div` fromIntegral maxThreads
      chunks = parMap rseq (\threadNo ->
         let localA = a + fromIntegral threadNo * fromIntegral localN * h
             localB = localA + fromIntegral localN * h
             in trapezoid f localA localB localN h) [0..maxThreads-1]
  print $ sum chunks

trapezoid :: (Double -> Double) -> Double -> Double -> Int -> Double -> Double
trapezoid f a b n h =
  h * foldl' (+) 0 (endPoints:internals)
  where
  endPoints = (f a + f b) / 2
  internals = map f $ take (n - 1) $ iterate (+h) (a + h)

f :: Double -> Double
f x = 4 / (1 + x * x)
