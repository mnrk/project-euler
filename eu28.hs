
eu28 = sum $ diagonals 1001
  where diagonals 1 = [1]
        diagonals n = map (\x -> n^2 - x * (n - 1)) [0, 1, 2, 3] ++ diagonals (n-2)

