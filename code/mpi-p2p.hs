module Main where

import Control.Parallel.MPI.Simple
import System (getArgs)
import Trapezoid

main :: IO ()
main = mpiWorld $ \numRanks rank -> do
  aStr:bStr:nStr:_ <- getArgs
  let [a,b] = map read [aStr,bStr]
      n = read nStr
      h = (b - a) / fromIntegral n
      localN = n `div` fromIntegral numRanks
      localA = a + fromIntegral rank * fromIntegral localN * h
      localB = localA + fromIntegral localN * h
      integral = trapezoid f localA localB localN h
      tag = 123 :: Tag
  if rank == 0
    then do 
    print . (+integral) . sum =<< sequence [ recv' commWorld (toRank proc) tag | proc <- [1..numRanks-1] ]
    else send commWorld 0 tag integral
  where
    recv' comm rank tag = do 
      (msg, status) <- recv comm rank tag
      return msg
