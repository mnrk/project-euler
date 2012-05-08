
import System.Environment (getArgs)

--1  1  1  1
--1  2  3  4
--1  3  6 10
--1  4 10 20

--[1,1,1,1,1] -> [1,2,3,4,5] -> [1,3,6,10,15] ...

paths :: Int -> [Integer]
paths 1 = [1,1..]
paths x = scanl1 (+) (paths (x-1))

eu15 :: Int -> Integer
eu15 x = (paths (x+1)) !! x

main = getArgs >>= print . eu15 . read . head