module Trapezoid (trapezoid, f) where

trapezoid :: (Double -> Double)  -- Function to integrate
             -> Double -> Double -- integration bounds
             -> Int              -- number of trapezoids
             -> Double           -- width of a trapezoid
             -> Double
trapezoid f a b n h =
   h * (endPoints + internals)
   where
   endPoints = (f a + f b) / 2.0
   internals = worker 0 (a + h) 0.0
   worker :: Int -> Double -> Double -> Double
   worker count x acc
      | count >= n - 1 = acc
      | otherwise = worker (count + 1) (x + h) (acc + f x)

f :: Double -> Double
f = sin

