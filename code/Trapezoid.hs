module Trapezoid (trapezoid, f) where

trapezoid :: (Double -> Double)
             -> Double -> Double
             -> Int
             -> Double
             -> Double
trapezoid f a b n h =
   h * (endPoints + internals)
   where
   endPoints = (f a + f b) / 2.0
   internals = worker 1 (a + h) 0.0
   worker :: Int -> Double -> Double -> Double
   worker count x acc
      | count == (n - 2) = acc
      | otherwise = worker (count + 1) (x + h) (acc + f x)

{-
trapezoid :: (Double -> Double)  -- Function to integrate
             -> Double -> Double -- integration bounds
             -> Int              -- number trapezoids
             -> Double           -- width of a trapezoid
             -> Double
trapezoid f a b n h =
  h * sum (endPoints:internals)
  where
  endPoints = (f a + f b) / 2
  internals = map f $ take (n - 1) $ iterate (+h) (a + h)
-}

f :: Double -> Double
f x = 4 / (1 + x * x)

