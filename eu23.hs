import Data.List
import Data.Array

main = print . sum $ [1..28122] \\ additions
  where additions = abundants `seq` [x+y | x <- abundants, y <- abundants, x+y < 28122]

    --ab = listArray (12,28122) $ abundants

abundants :: [Int]
abundants = filter isAbundant [12..28122]
  where isAbundant x = sum (divisors x) > x

divisors :: Int -> [Int]
divisors x = 1 : filter isDivisor [2..limit]
  where isDivisor a = x `mod` a == 0
        limit = x `div` 2