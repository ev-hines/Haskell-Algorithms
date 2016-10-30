import           Control.Monad.State
import           Criterion.Main
import           Data.List
import           Data.Ord
import           Debug.Trace
import           GreedySch
import           MaxInvestment
import           System.Random
import           Test.QuickCheck
-- B set to 1000, change all "1000" in code to the B desired to test.
instance Arbitrary Job where
    arbitrary = do
        st <- arbitrary
        en <- arbitrary
        return (Job ((st `mod` 1000), (en `mod` 1000)))

-- Compares the brute force O(n^2) algorithm to the O(max(Bn, nlogn)) algorithm.
-- If their lengths don't match, it fails and outputs the failing input. Note that
-- the jobs chosen may differ simply due to the fact that they may find equal length
-- schedules in different orders.
testSchedule :: Integer -> [Job] -> Bool
testSchedule b xs = (length $ circularGreedySchedule b
                    xs)
                    == (length $ circularGreedyScheduleBrute1 b $ xs)

ran :: State StdGen Integer
ran = state $ randomR(-100000000, 10000000000)

randomJob :: Integer -> State StdGen Job
randomJob b = do
    a <- ran
    c <- ran
    return (Job (a `mod` b, c `mod` b))


-- Runs 100,000 random tests comparing my optimized O(nlogn) algorithm
-- to the O(n^2) brute force algorithm.

-- Test the two investment finders with random  initial capital, f1, f2, and investment lists. Investment lists are limited to 1-y years due to the fact the size of CP of lists of lists
-- is i^y and thus will quickly eat up all the memory on a computer and take forever.
testInvest :: Int -> Int -> Investment -> Deduction -> Deduction -> [[Investment]] -> Bool
testInvest y i c' a' b' xs' = null xs || trace("\n" ++"\nCapital: " ++ show c ++ "\n" ++ "f1: " ++ show a
                                                               ++ "\n" ++ "f2: " ++ show b ++ "\nO(i*y) output: " ++ show iy
                                                               ++ "\nBrute output: " ++ show brute ++ "\nDifference: " ++ show (fst iy - fst brute) ++ "\n\n")
                                                        (fst iy == fst brute)
    where
          xs = take y $ (map . map) abs q
          -- Remove null values and attempt to normalize the input to ~[0, 1]
          xs''' = (map . map) (\ x -> if abs x <= 1 then abs x else 1.0/(1.5 * log (abs x + 1))) $ filter (not . null) xs'
          (a, b) = if abs a' > abs b' then (abs b', 30*abs a') else (abs a', 1.5*abs b')
          c = if c' == 0 then 10000 else 45*abs c' + 5000
          iy =  maxInvestment c a b xs
          brute = maxInvestmentBrute c a b xs
          q = if i <= 0 then map (take (length $ minimumBy (comparing length) xs''')) xs''' else map (take (length $ minimumBy (comparing length) $ map (take i) xs''')) xs'''


randS :: State StdGen Double
randS = state $ randomR(0, 1)

main :: IO ()
main = do
    putStrLn "Testing Scheduling Algorithm. 500-4000 jobs, 1000 hour \"day\"."
    q <- getStdGen
    quickCheckWith (stdArgs {maxSuccess = 10000}) (testSchedule 1000)
    let w1 = fst $ runState (replicateM 500 (randomJob 1000)) q
        w2 = fst $ runState (replicateM 1000 (randomJob 1000)) q
        w3 = fst $ runState (replicateM 2000 (randomJob 1000)) q
        w4 = fst $ runState (replicateM 4000 (randomJob 1000)) q
        k1 = fst $ runState (replicateM 2 (replicateM 1000 randS)) q
        k2 = fst $ runState (replicateM 4 (replicateM 1000 randS)) q
        k3 = fst $ runState (replicateM 8 (replicateM 1000 randS)) q
        k4 = fst $ runState (replicateM 16 (replicateM 1000 randS)) q
        k5 = fst $ runState (replicateM 32 (replicateM 1000 randS)) q
        k6 = fst $ runState (replicateM 64 (replicateM 1000 randS)) q
        k7 = fst $ runState (replicateM 128 (replicateM 1000 randS)) q
        k8 = fst $ runState (replicateM 256 (replicateM 1000 randS)) q
        k9 = fst $ runState (replicateM 512 (replicateM 1000 randS)) q
        k10 = fst $ runState (replicateM 1024 (replicateM 1000 randS)) q
        k11 = fst $ runState (replicateM 2048 (replicateM 1000 randS)) q
        k12 = fst $ runState (replicateM 4096 (replicateM 1000 randS)) q
        k13 = fst $ runState (replicateM 8192 (replicateM 1000 randS)) q
        
    putStrLn "Testing Investment Algorithm, 2-8192 years, 1000 investments per year"
    quickCheckWith (stdArgs {maxSuccess=10000}) (testInvest 4 4)

    -- Test in a larger list than quickCheck would come up with for speed.
    defaultMain [
        bgroup "circularGreedySchedule" [ bench "500"  $ whnf (circularGreedySchedule 1000) w1
                                        , bench "1000"  $ whnf (circularGreedySchedule 1000) w2
                                        , bench "2000"  $ whnf (circularGreedySchedule 1000) w3
                                        , bench "4000" $ whnf (circularGreedySchedule 1000) w4
                                        ]
       , bgroup "circularGreedyScheduleBrute1" [ bench "500"  $ whnf (circularGreedyScheduleBrute1 1000) w1
                                             , bench "1000"  $ whnf (circularGreedyScheduleBrute1 1000) w2
                                             , bench "2000"  $ whnf (circularGreedyScheduleBrute1 1000) w3
                                             , bench "4000" $ whnf (circularGreedyScheduleBrute1 1000) w4
                                              ]
      , bgroup "circularGreedyScheduleBrute2" [ bench "500"  $ whnf (circularGreedyScheduleBrute2 1000) w1
                                             , bench "1000"  $ whnf (circularGreedyScheduleBrute2 1000) w2
                                             , bench "2000"  $ whnf (circularGreedyScheduleBrute2 1000) w3
                                             , bench "4000" $ whnf (circularGreedyScheduleBrute2 1000) w4
                                              ]
      , bgroup "maxInvestment" [
                    bench "2"  $ whnf (maxInvestment 10000 500 2000) k1
                  , bench "4"  $ whnf (maxInvestment 10000 500 2000) k2
                  , bench "8"  $ whnf (maxInvestment 10000 500 2000) k3
                  , bench "16"  $ whnf (maxInvestment 10000 500 2000) k4
                  , bench "32"  $ whnf (maxInvestment 10000 500 2000) k5
                  , bench "64"  $ whnf (maxInvestment 10000 500 2000) k6
                  , bench "128"  $ whnf (maxInvestment 10000 500 2000) k7
                  , bench "256"  $ whnf (maxInvestment 10000 500 2000) k8
                  , bench "512"  $ whnf (maxInvestment 10000 500 2000) k9
                  , bench "1024"  $ whnf (maxInvestment 10000 500 2000) k10
                  , bench "2048"  $ whnf (maxInvestment 10000 500 2000) k11
                  , bench "4096"  $ whnf (maxInvestment 10000 500 2000) k12
                  , bench "8192"  $ whnf (maxInvestment 10000 500 2000) k13
                  ]
                  ]









