module Main where

import Control.Parallel.MPI.Simple
import System (getArgs)

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
  if rank == 0
     then print . sum =<< gatherRecv commWorld 0 integral
     else gatherSend commWorld 0 integral

trapezoid :: (Double -> Double) -> Double -> Double -> Int -> Double -> Double
trapezoid f a b n h =
  h * sum (endPoints:internals)
  where
  endPoints = (f a + f b) / 2
  internals = map f $ take (n - 1) $ iterate (+h) (a + h)

f :: Double -> Double
-- f x = 4 / (1 + x * x)
-- f = cos
f = const 1
