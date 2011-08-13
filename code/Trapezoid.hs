module Trapezoid (trapezoid, f) where

import Data.List

-- Integrate 'f' on interval [a,b] using 'n' steps of size 'h'
trapezoid :: (Double -> Double) -> Double -> Double -> Int -> Double -> Double
trapezoid f a b n h =
  h * foldl' (+) 0 (endPoints:internals)
  where
  endPoints = (f a + f b) / 2
  internals = map f $ take (n - 1) $ iterate (+h) (a + h)

f :: Double -> Double
f x = 4 / (1 + x * x)
-- f = cos
-- f = const 1
