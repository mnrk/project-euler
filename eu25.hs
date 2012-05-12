
fib = 1 : 1 : zipWith (+) fib (tail fib)

eu25 = head . dropWhile not1000Digits $ injectIndex fib
  where not1000Digits = (< 1000) . length . show . fst
        injectIndex a = zipWith (\x y -> (x,y)) a [1..]
