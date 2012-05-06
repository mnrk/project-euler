import Data.List.Split
import Control.Monad

type Matrix = [[Int]]
type Coord = (Int,Int)
type CoordCombo = [Coord]

eu11 = do
  matrix <- eu11data
  return $ maximum (map (mkProduct matrix) (mkCombos 20 20 4))

eu11data :: IO Matrix
eu11data = do
  a <- readFile "data/eu11.txt"
  let readInt x = read x :: Int
  return $ map (map readInt . splitOn " ") (lines a)

mkProduct :: Matrix -> CoordCombo -> Int
mkProduct matrix combo = foldl1 (*) (combo2Numbers matrix combo)

combo2Numbers :: Matrix -> CoordCombo -> [Int]
combo2Numbers matrix combo = map (\(x,y) -> (matrix !! x) !! y) combo  

mkCombos :: Int -> Int -> Int -> [CoordCombo]
mkCombos maxX maxY size = horizontal ++ vertical ++ lrDiagonal ++ rlDiagonal
  where
    horizontal = do
      coorX <- [0..(maxX-size)]
      coorY <- [0..(maxY-1)]
      return [(x,y) | x <- [coorX..(coorX+size-1)], y <- [coorY]]
    vertical = do
      coorX <- [0..(maxX-1)]
      coorY <- [0..(maxY-size)]
      return [(x,y) | x <- [coorX], y <- [coorY..(coorY+size-1)]]
    lrDiagonal = do
      coorX <- [0..(maxX-size)]
      coorY <- [0..(maxY-size)]
      return [(coorX+s,coorY+s) | s <- [0..(size-1)]]
    rlDiagonal = do
      coorX <- [0..(maxX-size)]
      coorY <- [(size-1)..(maxY-1)]
      return [(coorX+s,coorY-s) | s <- [0..(size-1)]]
