
import Data.Char

decimal :: String -> Int
decimal x = foldl1 (+) $ zipWith (*) (map digitToInt $ reverse x) (iterate (*2) 1)

binary :: Int -> Int -> String
binary base n =  map (intToDigit . fst) . tail $ scanl (\(_,q) x -> q `quotRem` x) (0,n) mults
  where mults = reverse . takeWhile (<= n) . iterate (*base) $ 1

isPalin :: Show a => a -> Bool
isPalin x = let s = show x in s == reverse s

palins :: [Int]
palins = evenPalins ++ oddPalins ++ singleDigit
  where evenPalins = [let s = show x in read (s ++ reverse s) | x <- [1..999]]
        oddPalins  = [let { s = show x; t = show y } in read (s ++ t ++ reverse s) | x <- [1..99], y <- [0..9]]
        singleDigit = [1..9]

eu36 = sum . map decimal . filter isPalin $ map (binary 2) palins