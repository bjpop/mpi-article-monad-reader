module Main where

import Control.Parallel.MPI.Simple
import System (getArgs)
import Trapezoid

main :: IO ()
main = mpi $ do
  numRanks <- commSize commWorld
  rank <- commRank commWorld
  let master = 0 :: Rank
      
  aStr:bStr:nStr:_ <- getArgs
  let [a,b] = map read [aStr,bStr]
      n = read nStr
      h = (b - a) / fromIntegral n
      localN = n `div` fromIntegral numRanks
      localA = a + fromIntegral rank * fromIntegral localN * h
      localB = localA + fromIntegral localN * h
      integral = trapezoid f localA localB localN h
  if rank == master then do 
    rest <- sequence [ recv' commWorld (toRank proc) unitTag 
                     | proc <- [1..numRanks-1] ]
    print (integral + sum rest)
    else send commWorld master unitTag integral
  where
    recv' comm rank tag = do 
      (msg, status) <- recv comm rank tag
      return msg
