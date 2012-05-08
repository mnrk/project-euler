
import Data.Char

eu20 = sum . (map digitToInt . show) . product $ [1..100]