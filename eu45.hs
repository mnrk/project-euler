
triangles   = map (\n -> n*(n+1) `div` 2) [1..]
pentagonals = map (\n -> n*(3*n-1) `div` 2) [1..]
hexagonals  = map (\n -> n*(2*n-1)) [1..]

shared (x:xs) (y:ys) | x == y    = x : (shared xs ys)
                     | x < y     = shared xs (y:ys)
                     | otherwise = shared (x:xs) ys

eu45 = shared (shared triangles pentagonals) hexagonals !! 2