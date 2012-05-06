eu13 :: IO String
eu13 = do
  a <- readFile "data/eu13.txt"
  let arr =  (map read) . lines $ a
  return . take 10 . show . sum $ arr
