import qualified Data.Set as S
import Data.List
import Primes (primes)

-------------------------------------------------------------------------------

primeSet = S.fromList $ takeWhile (<80000) primes

combos = [(a,b) | a <- [-999..999], b <- [-999..999]]

countPrimes :: (Int, Int) -> ((Int, Int), Int)
countPrimes (a,b) = ((a,b), result)
  where result      = length . takeWhile isPrime $ map condition [0..]
        isPrime x   = S.member x primeSet
        condition n = n^2 + a * n + b

eu27 = (\((a,b), _) -> a * b) . maximumBy cmp $ map countPrimes combos
  where cmp x y = compare (snd x) (snd y)
