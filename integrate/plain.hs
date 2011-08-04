module Main where

approx f xs ws = sum [w * f x | (x,w) <- zip xs ws]
 
integrateOpen :: Fractional a => a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateOpen v vs f a b n = approx f xs ws * h / v where
  m = fromIntegral (length vs) * n
  h = (b-a) / fromIntegral m
  ws = concat $ replicate n vs
  c = a + h/2
  xs = [c + h * fromIntegral i | i <- [0..m-1]]
 
integrateClosed :: Fractional a => a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateClosed v vs f a b n = approx f xs ws * h / v where
  m = fromIntegral (length vs - 1) * n
  h = (b-a) / fromIntegral m
  ws = overlap n vs
  xs = [a + h * fromIntegral i | i <- [0..m]]
 
overlap :: Num a => Int -> [a] -> [a]
overlap n []  = []
overlap n (x:xs) = x : inter n xs where
  inter 1 ys     = ys
  inter n []     = x : inter (n-1) xs
  inter n [y]    = (x+y) : inter (n-1) xs
  inter n (y:ys) = y : inter n ys

intLeftRect, intMidRect, intRightRect, intTrapezium, intSimpson :: Fractional a => (a -> a) -> a -> a -> Int -> a
intLeftRect  = integrateClosed  1 [1,0]
intMidRect   = integrateOpen    1 [1]
intRightRect = integrateClosed  1 [0,1]
intTrapezium = integrateClosed  2 [1,1]
intSimpson   = integrateClosed  3 [1,4,1]

methods :: Fractional a => [(String, (a->a) -> a -> a -> Int -> a)]
methods = 
  [ ( "rectangular left:    ", intLeftRect  )
  , ( "rectangular middle:  ", intMidRect   )
  , ( "rectangular right:   ", intRightRect )
  , ( "trapezium:           ", intTrapezium )
  , ( "simpson:             ", intSimpson   )
  ]
  
-- Integrate f on [a,b] with n steps using all available methods, print results
tabulate :: Fractional a => (a->a) -> a -> a -> Int -> [String]
tabulate f a b n =
  map (\(name, method) -> name ++ (show $ method f a b n)) methods

-- Integrate several sample functions
tabulateSeveral =
  map (unlines . uncurry4 tabulate)
  [ ((\x -> x ^ 3), 0, 1   , 1000000)
  , ((\x -> 1 / x), 1, 100 , 1000000)  
  , ((\x -> x)    , 0, 5000, 5000000)
  , ((\x -> x)    , 0, 6000, 6000000)
  ]
  where
    uncurry4 f (a,b,c,d) = f a b c d
  
main = do
  putStrLn $ unlines $ tabulateSeveral
