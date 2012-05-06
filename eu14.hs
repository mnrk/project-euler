
import Data.List
import Data.Array
import Data.Ord

run n = arr
  where
    arr = listArray (1,n) $ 0 : [1 + solve n x | x <- [2..n]]
    solve n x
      | stepx <= n = arr ! stepx
      | otherwise   = 1 + (solve n stepx)
          where stepx = step x
    step x
      | even x    = x `div` 2
      | otherwise = 3 * x + 1

main = putStrLn . show $ maximumBy (comparing snd) (assocs (run 50))