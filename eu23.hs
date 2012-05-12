import Data.List
import Data.Array
import System.Environment

main = print . sum . filter notAddsMember $ [1..28123]
  where notAddsMember x = not $ addsMembers ! x

addsMembers :: Array Int Bool
addsMembers = accumArray (||) False (1,28123) $ map (\x -> (x,True)) additions

additions :: [Int]
additions = abundants `seq` [x+y | x <- abundants, y <- abundants, x+y <= 28123]

abundants :: [Int]
abundants = filter isAbundant [12..28123]
  where isAbundant x = sum (divisors x) > x

divisors :: Int -> [Int]
divisors x = 1 : filter isDivisor [2..limit]
  where isDivisor a = x `mod` a == 0
        limit = x `div` 2
