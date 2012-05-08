
import System.Environment (getArgs)

--0 1 1 1
--1 2 3 4
--1 3 6 10
--1 4 10 20

paths :: (Int,Int) -> Int
paths (1,1) = 0
paths (x,1) = 1
paths (1,y) = 1
paths (x,y) = paths (x-1,y) + paths (x,y-1)

main = getArgs >>= print . (\x -> paths (x,x)) . read . head