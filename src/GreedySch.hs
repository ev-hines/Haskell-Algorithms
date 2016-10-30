{-# LANGUAGE FlexibleContexts #-}
module GreedySch
    (
        Job(..)
      , circularGreedySchedule -- O(max(min(Bn, n^2), nlogn))
      , circularGreedyScheduleBrute1 -- O(n^2) but better than brute 2
      , circularGreedyScheduleBrute2 -- O(n^2)
    ) where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Ord

{-
Solves the scheduling optimization problem modulo some B in O(n log n) time.
The O(n log n) algorithm uses the fact that we need only
check for each rotation p such that 0 <= p < B instead of every node.
The O(n^2) algorithm simply does a rotation for each node and thus
checks the same starting hour multiple times.

Additionally, it contains a test harness for comparing my O(n log n)
algorithm to the brute force O(n^2) algorithm over random inputs.
-}

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
   b <- rpar (f a)
   bs <- parMap' f as
   return (b:bs)

newtype Job = Job { interval :: (Integer, Integer) } deriving (Ord, Eq, Show)

rotate :: Integer -> Integer -> Job -> Job
rotate b p (Job (x, y)) = Job ((x - p) `mod` b, (y - p) `mod` b)

-- Greedy sort with 0 as a maximum finish time
greedySort :: [Job] -> [Job]
greedySort = sortBy (\ (Job (_, y1)) (Job (_, y2)) -> if y1 < y2 && y1 /= 0 && y2 /= 0
                                                      then LT else if y1 == y2 then GT
                                                      else if y1 == 0 then GT else
                                                      if y2 == 0 then LT else GT)

removeCrossEvents :: [Job] -> [Job]
removeCrossEvents = filter (\ (Job (c, d)) -> c < d || d == 0)

removeDups :: [Job] -> [Job]
removeDups (a@(Job (x, _)):b@(Job(x2, _)):xs)
    | x == x2 = removeDups (b:xs)
    | otherwise = a : removeDups (b:xs)
removeDups [x] = [x]
removeDups [] = []

-- Greedy schedule
greedySchedule :: Integer -> Job -> [Job] -> [Job]
greedySchedule b p = runEval . parMap' (rotate b (-rot))  . greedySchedule' (-100000)  .
                     removeCrossEvents . runEval . parMap' (rotate b rot)
    where
          rot = fst . interval $ p
         -- Greedy schedule with 0 as the maximum finish time
          greedySchedule' f (a@(Job (x, y)):xs)
                | x >= f && f /= 0 = a : greedySchedule' y xs
                | otherwise = greedySchedule' f xs
          greedySchedule' _ _ = []

-- runs the greedySchedule algorithm at 0 and a single
-- cross term for each p such that 0 <= p < B
-- O(max(min(Bn, n^2), nlogn)) = O(nlogn) for constant B
circularGreedySchedule :: Integer ->  [Job] -> [Job]
circularGreedySchedule b xs' = if null v && not (null xs) then [head xs] else v
    where xs = greedySort xs'
          v = maximumBy (comparing length) (noCross:efc)
          cross = removeDups . sortBy (comparing (fst . interval)) $
                  filter (\ (Job (x, y)) -> x > y) xs :: [Job]
          noCross = greedySchedule b (Job (0, 0)) xs
          efc =  runEval $ parMap' (\ x -> greedySchedule b x xs) cross
          
-- runs the greedySchedule algorithm starting at
-- every possible cross node O(n^2)
circularGreedyScheduleBrute1 :: Integer -> [Job] -> [Job]
circularGreedyScheduleBrute1 b xs' = if null v && not (null xs) then [head xs] else v
    where xs = greedySort xs'
          v = maximumBy (comparing length) (noCross:efc)
          cross =
                  filter (\ (Job (x, y)) -> x > y) xs :: [Job]
          noCross = greedySchedule b (Job (0, 0)) xs
          efc =  runEval $ parMap' (\ x -> greedySchedule b x xs) cross


-- runs the greedySchedule algorithm starting at
-- every possible job O(n^2)
circularGreedyScheduleBrute2 :: Integer -> [Job] -> [Job]
circularGreedyScheduleBrute2 b xs' = if null v && not (null xs) then [head xs] else v
    where xs = greedySort xs'
          v = maximumBy (comparing length) (noCross:efc)
          cross =
                  filter (\ (Job (x, y)) -> x > y) xs :: [Job]
          noCross = greedySchedule b (Job (0, 0)) xs
          efc =  runEval $ parMap' (\ x -> greedySchedule b x xs) xs
