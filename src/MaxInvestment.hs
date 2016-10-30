module MaxInvestment(
  maxInvestment
, maxInvestmentBrute
, Investment
, Capital
, Deduction
  ) where 
import Data.List
import Data.Ord

type Investment = Double
type Capital = Double
type Deduction = Double


-- Find maximum investment in O(i*y) time
-- Assumes you pay when you invest. Thus your total investment with $i is (i - f) and
-- your return is (i - f) * r in contrast to if you paid after each year which would be
-- i * r - f. This works because we keep a solution vector such that v_i = the maximum
-- capital possibly available such that you invested in the ith investment in the previous
-- year along with the path we took to get there. Note that only the max(v) is the actual
-- optimal solution, all others are simply the optimal solutions if we HAD to invest in
-- the ith investment.
maxInvestment :: Capital -> Deduction -> Deduction  -> [[Investment]] -> (Capital, [Int])
maxInvestment c _ _ [] = (c, [])
maxInvestment c f1' f2' (i:is') = maxInvestment' cs' f1' f2'  ((map . map) (+1) is')
    where cs'' = replicate (length i) (c, [])
          i' = map (+1) i :: [Investment]
          cs' = map (\ (idx, x, inv) -> (idx, (x * inv, [idx]))) $ zip3 [(0 :: Int)..] (map fst cs'') (i' :: [Investment])
          maxInvestment' ::[(Int, (Capital, [Int]))] -> Deduction -> Deduction  -> [[Investment]] -> (Capital, [Int])
          maxInvestment' cs f1 f2 is = (\ (_, (x, y)) -> (x, reverse y)) $ maximumBy (comparing (fst . snd))
                                       $ foldl' (\ acc inv -> 
                                                  let (idm, (maxCS, li)) = maximumBy (comparing (fst . snd)) acc
                                                  in zipWith (\ (idx, (x, li')) v -> if (maxCS - f2) * v > (x - f1) * v && idx /= idm
                                                                                     then  (idx, ((maxCS - f2) * v, idx:li))
                                                                                     else (idx, ((x-f1)*v, idx:li'))) acc inv) cs is

-- Brute force the max investment by trying all possible combinations.
maxInvestmentBrute :: Capital -> Deduction -> Deduction  -> [[Investment]] -> (Capital, [Int])
maxInvestmentBrute c f1 f2 is = maximumBy (comparing fst) . map ((\ (_, y, z) -> (y, reverse z))
                                . foldl' (\ (s, t, li) (q, r) -> if s == -1 then (q, t * r, [q]) else if s == q
                                                                 then (q, (t - f1)*r, s:li)
                                                                 else (q, (t - f2)*r, q:li)) (-1, c, []))
                                  $ mapM (zip [(0 :: Int)..] . map (+1)) is
