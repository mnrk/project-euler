import Data.List
import Debug.Trace

perms :: Int -> [Int] -> [Int]
perms 1 xs = xs
perms pos (x:xs)
  | xsCombos >= pos  = x : perms pos xs
  | otherwise       = perms (pos - xsCombos) (y:ys)
    where factorial x = foldl1 (*) [1..x]
          xsCombos = factorial (length xs)
          y = minimum $ filter (> x) xs
          ys = sort $ x : filter (/= y) xs

main = print . concat . map show . perms 1000000 $ [0..9]
