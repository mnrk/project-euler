import Data.String.Utils

replacements = [("VIIII","IX"),("IIII","IV"),("LXXXX","XC"),("XXXX","XL"),("DCCCC","CM"),("CCCC","CD")]

main = do
  file <- readFile "data/roman.txt"
  let content    = map strip . lines $ file
      minimize r = foldl (\x (u,m) -> replace u m x) r replacements
      minimized  = map minimize content 
    in return . foldl1 (-) . map (length . concat) $ [content, minimized]