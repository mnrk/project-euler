import Data.Char

eu30 = sum [x | x <- [10..limit], x == sumPwrs x]
  where digits    = last . takeWhile (\x -> x * 9^5 > 10^(x - 1)) $ [1..]
        limit     = 10^digits - 1
        digitPwrs = map (^5) [0..9]
        pwrDig a  = digitPwrs !! (digitToInt a)
        sumPwrs a = sum . map pwrDig $ show a
