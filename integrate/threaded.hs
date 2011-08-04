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
integrate :: Fractional a => (a->a) -> a -> a -> Int -> IO ()
integrate f a b n = do
  putStrLn $ unlines [ name ++ (show $ method f a b n) | (name, method) <- methods ]
  putStrLn ""

main = do
  integrate (\x -> x ^ 3) 0 1    100
  integrate (\x -> 1 / x) 1 100  1000
  integrate (\x -> x)     0 5000 5000000
  integrate (\x -> x)     0 6000 6000000
