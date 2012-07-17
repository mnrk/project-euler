module Primes
( primes
, isFactor
, squareRoot
) where

-------------------------------------------------------------------------------

squareRoot :: (Integral a) => a -> a
squareRoot = floor . sqrt . (fromIntegral :: (Integral a) => a -> Double)

primes :: Integral a => [a]
primes = 2 : iterate nextPrime 3

nextPrime :: Integral a => a -> a
nextPrime last
  | any (isFactor start) (takeWhile (<= squareRoot start) primes) = nextPrime start
  | otherwise = start
      where start = last + 2

isFactor :: Integral a => a -> a -> Bool
isFactor a b = a `mod` b == 0
