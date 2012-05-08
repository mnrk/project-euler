
import Data.List.Split
import Data.List
import Data.Char

main = readFile "data/eu22.txt" >>= print . sum . (map score) . label . sort . parse

parse :: String -> [String]
parse = (splitOn ",") . (filter (`elem` ',':['A'..'Z']))

score :: (String,Int) -> Int
score (x,n) = sum (map position x) * n
  where position a = ord a - ord 'A' + 1

label :: [String] -> [(String,Int)]
label = zipWith (\a b -> (b,a)) [1,2..]