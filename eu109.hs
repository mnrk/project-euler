
import Data.Array

numbers = [1..20]

regions = listArray (1,63) $ 0:25:numbers ++ map (3*) numbers ++ 50 : map (2*) numbers

eu109 = length $ [[x,y,z] | z <- [43..63], y <- [1..63], x <- [y..63], isCheckout x y z]

isCheckout x y z = sum (map (regions !) [x, y, z]) < 100