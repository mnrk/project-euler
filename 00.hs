import Control.Monad
import Data.List
import Data.Char

-----------------------------------------------------------
--
-- 1
--
-- If we list all the natural numbers below 10 that are 
-- multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of 
-- these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-----------------------------------------------------------

eu1 = sum [x | x <- [1..999], any (isFactor x) [5,3]]

isFactor :: Integral a => a -> a -> Bool
isFactor a b = a `mod` b == 0

-----------------------------------------------------------
--
-- 2
--
-- Each new term in the Fibonacci sequence is generated by 
-- adding the previous two terms. By starting with 1 and 2,
-- the first 10 terms will be:
--
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
-- By considering the terms in the Fibonacci sequence whose
-- values do not exceed four million, find the sum of the 
-- even-valued terms.
--
-----------------------------------------------------------

eu2 = sum fibs
  where nextFib (a,b,c) = (b,c,b+c)
        fibs = takeWhile (< 4000000) [x | (_,_,x) <- iterate nextFib (0,1,1), even x]

-----------------------------------------------------------
--
-- 3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 
-- 600851475143 ?
--
-----------------------------------------------------------

eu3 x = last . primeFactors $ x

primes :: Integral a => [a]
primes = 2 : iterate nextPrime 3

nextPrime :: Integral a => a -> a
nextPrime last
  | any (isFactor start) (takeWhile (<= squareRoot start) primes) = nextPrime start
  | otherwise = start
      where start = last + 2

primeFactors :: Integral a => a -> [a]
primeFactors x = case factor of
              Nothing -> [x]
              Just f  -> f : primeFactors (x `div` f)
            where 
              factor = find (isFactor x) (takeWhile (<= squareRoot x) primes)

squareRoot :: (Integral a) => a -> a
squareRoot = floor . sqrt . (fromIntegral :: (Integral a) => a -> Double)

isPrime :: (Integral a) => a -> Bool
isPrime a = last (takeWhile (<= a) primes) == a

-----------------------------------------------------------
--
-- 4
--
-- A palindromic number reads the same both ways. The 
-- largest palindrome made from the product of two 2-digit 
-- numbers is 9009 = 91 99.
--
-- Find the largest palindrome made from the product of two 
-- 3-digit numbers.
--
-----------------------------------------------------------

eu4 = maximum palindromes
  where 
    palindromes = do
      x <- [100..999]
      y <- [100..999]
      let p = x * y
      guard (show p == (reverse . show $ p))
      return p

-----------------------------------------------------------
--
-- 5
--
-- 2520 is the smallest number that can be divided by each
-- of the numbers from 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly 
-- divisible by all of the numbers from 1 to 20?
--
-----------------------------------------------------------

eu5 = foldl1 (*) . foldl1 merge $ map primeFactors [2..20]

merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge xs ys = merge' (sort xs) (sort ys)

merge' :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge' [] [] = []
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys)
  | x == y     = x : merge' xs ys
  | x < y      = x : merge' xs (y:ys)
  | otherwise  = y : merge' (x:xs) ys

-----------------------------------------------------------
--
-- 6
--
-- The sum of the squares of the first ten natural numbers is,
--   1^2 + 2^2 + ... + 10^2 = 385
--
-- The square of the sum of the first ten natural numbers is,
--   (1 + 2 + ... + 10)^2 = 552 = 3025
--
-- Hence the difference between the sum of the squares of 
-- the first ten natural numbers and the square of the sum 
-- is 3025 - 385 = 2640.
--
-- Find the difference between the sum of the squares of
-- the first one hundred natural numbers and the square of 
-- the sum.
--
-----------------------------------------------------------

eu6 = (square . sum) naturals - sum (map square naturals)
  where square x = x * x
        naturals = [1..100]

-----------------------------------------------------------
--
-- 7
--
-- By listing the first six prime numbers: 
--   2, 3, 5, 7, 11, and 13, we can see that the 6th prime 
-- is 13.
--
-- What is the 10 001st prime number?
--
-----------------------------------------------------------

eu7 = (filter isPrime [1..]) !! 10001


-----------------------------------------------------------
--
-- 8
--
--Find the greatest product of five consecutive digits in the 1000-digit number.
--
--   73167176531330624919225119674426574742355349194934
--   96983520312774506326239578318016984801869478851843
--   85861560789112949495459501737958331952853208805511
--   12540698747158523863050715693290963295227443043557
--   66896648950445244523161731856403098711121722383113
--   62229893423380308135336276614282806444486645238749
--   30358907296290491560440772390713810515859307960866
--   70172427121883998797908792274921901699720888093776
--   65727333001053367881220235421809751254540594752243
--   52584907711670556013604839586446706324415722155397
--   53697817977846174064955149290862569321978468622482
--   83972241375657056057490261407972968652414535100474
--   82166370484403199890008895243450658541227588666881
--   16427171479924442928230863465674813919123162824586
--   17866458359124566529476545682848912883142607690042
--   24219022671055626321111109370544217506941658960408
--   07198403850962455444362981230987879927244284909188
--   84580156166097919133875499200524063689912560717606
--   05886116467109405077541002256983155200055935729725
--   71636269561882670428252483600823257530420752963450
--
-----------------------------------------------------------

eu8 :: (Integral a) => a -> Int
eu8 x = maxProduct . map digitToInt $ show x

maxProduct :: (Num a, Ord a) => [a] -> a
maxProduct (a:b:c:d:e:[]) = a * b * c * d * e
maxProduct xs = max (foldl1 (*) (take 5 xs)) (maxProduct . tail $ xs)

-----------------------------------------------------------
--
-- 9
--
-- A Pythagorean triplet is a set of three natural numbers,
-- a  b  c, for which,
--     a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which 
--     a + b + c = 1000.
--
-- Find the product abc.
--
-----------------------------------------------------------

eu9 = nub a
  where 
    a = do
      x <- [1..500]
      y <- [1..500]
      let z = squareRoot (sq x + sq y)
      guard (sq x + sq y == sq z && x + y + z == 1000)
      return (x * y * z)
        where sq a = a * a

-----------------------------------------------------------
--
-- 10
--
-- Find the greatest product of five consecutive digits in 
-- the 1000-digit number.
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--
-----------------------------------------------------------

eu10 = sum $ filter isPrime [2..1999999]