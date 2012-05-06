
import Data.List
import Data.Array
import Data.Ord

run :: Int -> Array Int Int
run n = arr
  where
    arr = listArray (1,n) $ 0 : [solve n x | x <- [2..n]]
    solve n x
      | firstStep <= n = 1 + (arr ! firstStep)
      | otherwise      = 1 + solve n firstStep
          where firstStep = step x

step :: Int -> Int
step x
  | even x    = x `div` 2
  | otherwise = 3 * x + 1

main = putStrLn . show $ maximumBy (comparing snd) (assocs (run 999999))