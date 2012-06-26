
eu31 = length $ walk [200, 100, 50, 20, 10, 5, 2] 0

walk [] v = [[200 - v]]
walk (n:ns) v = concat . map (\x -> map (x :) (walk ns (n * x + v))) $ [0..(200-v) `div` n]
