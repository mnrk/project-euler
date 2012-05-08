
import Data.Array

run = sum $ filter amicable [1..10000]
  where d = 1 : map (sum . divisors) [1..]
        amicable a = let b = d !! a in a /= b && d !! b == a

divisors x = 1 : filter isDivisor [2..limit]
  where isDivisor a = x `mod` a == 0
        limit = x `div` 2