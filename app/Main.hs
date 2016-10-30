
import           GreedySch
import           MaxInvestment
main :: IO ()
main = do
    print $ circularGreedySchedule 24 $  map Job [(3, 4), (4, 5), (4, 12), (11, 15), (15, 4), (15, 3)]
    print $ maxInvestment 10000 200 400 [[0.5, 0.7, 0.8], [0.7, 0.6, 0.7], [0.4, 0.9, 0.5]]
