import System.Environment (getArgs)

main = getArgs >>= readFile . head >>= print . eu18 . parse . map words . lines

eu18 :: [[Int]] -> Int
eu18 a = head $ foldr1 solve a

solve :: [Int] -> [Int] -> [Int]
solve [] [_] = []
solve (x:xs) (z:y:ys) = (x + max z y) : solve xs (y:ys)

parse :: [[String]] -> [[Int]]
parse = map (map read)
