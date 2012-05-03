import Control.Monad
import Data.List

--1--
eu1 = sum [x | x <- [1..999], any (isFactor x) [5,3]]

isFactor :: Integral a => a -> a -> Bool
isFactor a b = a `mod` b == 0

--2--
eu2 = sum fibs
  where nextFib (a,b,c) = (b,c,b+c)
        fibs = takeWhile (< 4000000) [x | (_,_,x) <- iterate nextFib (0,1,1), even x]

--3--
eu3 x = primeFactor x (squareRoot x)

primeFactor :: Integral a => a -> a -> a
primeFactor x d = if isFactor x d && isPrime d
                    then d
                    else primeFactor x (d-1)

squareRoot :: (Integral a) => a -> a
squareRoot = floor . sqrt . (fromIntegral :: (Integral a) => a -> Double)

isPrime :: (Integral a) => a -> Bool
isPrime a = all (not . isFactor a) [2..(squareRoot a)]

--4--
eu4 = maximum palindromes
  where 
    palindromes = do
      x <- [100..999]
      y <- [100..999]
      let p = x * y
      guard (show p == (reverse . show $ p))
      return p

--5--
eu5 = foldl1 (*) . foldl1 merge $ map factors [2..20]

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

factors :: (Integral a) => a -> [a]
factors x
  | isPrime x = [x]
  | otherwise = p:(factors (x `div` p))
    where p = primeFactor x (squareRoot x)