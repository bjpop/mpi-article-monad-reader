module Main where

import GHC.Conc
import Control.Parallel.Strategies

import System (getArgs)
import Trapezoid

main :: IO ()
main = do
  let numThreads = numCapabilities
  aStr:bStr:nStr:_ <- getArgs
  let [a,b] = map read [aStr,bStr]
      n = read nStr
      h = (b - a) / fromIntegral n
      localN = n `div` fromIntegral numThreads
      chunks = parMap rseq (\threadNo ->
         let localA = a + fromIntegral (threadNo * localN) * h
             localB = localA + fromIntegral localN * h
             in trapezoid f localA localB localN h) [0..numThreads-1]
  print (sum chunks)
