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

applyTupple :: Num a => ((a -> a), (a -> a)) -> (a,a) -> (a,a)
applyTupple (fnL,fnR) (l,r) = (fnL l,fnR r)

mkCombos :: Int -> Int -> Int -> [CoordCombo]
mkCombos maxX maxY size = do
  coorX <- [0..maxX-1]
  coorY <- [0..maxY-1]
  direction <- [((+1),id), (id,(+1)), ((+1),(+1)), ((+1),(subtract 1))]
  let combos = [(x,y) | (x,y) <- take size $ iterate (applyTupple direction) (coorX,coorY)]
  guard (all (\(a,b) -> 0 <= a && a < maxX && 0 <= b && b < maxY) combos)
  return combos
