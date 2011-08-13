module Main where

import GHC.Conc
import Control.Parallel.Strategies

import System (getArgs)
import Trapezoid

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
  print (sum chunks)
