import Data.Char
import Data.List (sort)
import Data.List.HT (allEqual)

sameDigits xs = allEqual $ map (sort . show) xs

main = print . show $ head [x | a <- [1..], x <- [10^a..10^(a+1) `div` 6], sameDigits . map ($2) $ map (*) [2,3..6]]
