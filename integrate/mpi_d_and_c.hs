module Main where

import Control.Parallel.Strategies
import Control.Parallel.MPI.Simple
import Control.Applicative
import Text.Printf

type Process = Rank
type Allocator resource = resource -> [Rank] -> [( [resource],[Rank] )]
type Processor resource result = [resource] -> [Rank] -> [result]
type Combiner result total = [result] -> total 

data WorkItem item result total = 
  WorkItem { allocate :: Allocator item
           , process  :: Processor item result
           , combine  :: Combiner result total 
           }

d_and_c _    _    _       []        _      = error "Out of processes"
d_and_c _    _    _       _         []     = error "Out of tasks"
d_and_c comm root combine [process] [task] = do
  putStrLn $ printf "Executing single task at node %d" (fromRank process :: Int)
  r <- commRank comm
  if r == process then do 
    result <- task
    gatherSend comm root (combine [result])
    else return ()
         
d_and_c comm root combine ps ts | length ps < length ts = do
  let chunks = divide ts (length ps)
  r <- commRank comm
  if r `notElem` ps 
    then return ()
    else do
      my_results <- combine <$> sequence [ t | t <- (chunks!!(fromRank r))]
      if r == root 
        then do
        results <- gatherRecv comm root my_results    
        return $ combine results
        else gatherSend comm root my_results
    
d_and_c comm root combine ps ts | length ps > length ts = do
  let pgroups = divide ps (length ts)
  let leaders = map head pgroups
  let leader = head leaders
  r <- commRank comm
  if r `notElem` leaders
    then return ()
    else do
    let my_group = undefined
    let my_task = undefined
    my_result <- d_and_c comm r combine my_group [my_task]
    if r == leader
       then do
        results <- gatherRecv comm leader my_result
        return $ combine results
        else gatherSend comm leader my_result

divide lst n =
  let len = length lst 
      chunk_size = len `div` n 
      m = n + (if len `mod` n == 0 then 0 else 1)
      in
   reverse $ (\(a:b:rest) -> (a++b):rest) $ reverse $ map (take chunk_size) $ take m $ iterate (drop chunk_size) lst 
{-
d_and_c :: (Allocator resource) -> (Processor resource result) -> (Combiner result total) -> resource -> [Rank] -> IO total
d_and_c allocator processor combiner resource processes 
 | length processes == 1 = do
   
  = do
  let jobs = allocator resource processes
  let leader = head processes
  results <- doAndReportToLeader leader jobs
  return $ combiner results
  where
    doAndReportToLeader leader jobs
-}
{-  
doTasks processes =
  let pgroups = divide tasks processes
  unlines $ zipWith doTask pgroups tasks

doTask pgroup t = doMethods pgroup

doMethods processes =
  let pgroups = divide methods processes
  unlines $ zipWith doMethod pgroups methods

doMethod pgroup m = doIntegrate pgroup range f

doIntegrate pgroup range f =
  let ranges = divide range (len pgroup)
  sum $ zipWith inegrateOne ranges pgroup
      
integrateOne = ...
-}
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
  map (\(name, method) -> name ++ (show $ method f a b n)) methods `using` parList rdeepseq

-- Integrate one of sample functions
tabulateSome num =
  unlines . uncurry4 tabulate $ 
  [ ((\x -> x ^ 3), 0, 1   , 1000000)
  , ((\x -> 1 / x), 1, 100 , 1000000)  
  , ((\x -> x)    , 0, 5000, 5000000)
  , ((\x -> x)    , 0, 6000, 6000000)
  ] !! num
  where
    uncurry4 f (a,b,c,d) = f a b c d
  
main = mpi $ do
  size <- commSize commWorld
  rank <- commRank commWorld
  let root = 0 :: Rank
  let my_results = tabulateSome (fromRank rank)
  if rank == root then 
    do results <- gatherRecv commWorld root my_results
       putStrLn $ unlines $ results
    else gatherSend commWorld root my_results
    
