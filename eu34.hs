import Data.Char

limit :: Int
limit = (10^) . head $ dropWhile (\x -> x*362880 >= 10^x) [1..]

main = print $ sum [n | n <- [10..limit], isCurious n]

fact :: Int -> Int
fact 0 = 1
fact x = foldl1 (*) [1..x]

isCurious :: Int -> Bool
isCurious x = x == (sum . map (fact . digitToInt) $ show x)
