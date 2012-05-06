import Data.List.Split
import Control.Monad
import Data.Array

type Coord = (Int,Int)
type Matrix = Array Coord Int
data Dir = Dir {fnX :: (Int -> Int), fnY :: (Int -> Int)}

eu11 :: IO Int
eu11 = do
  a <- readFile "data/eu11.txt"
  let arr = listArray ((1,1),(20,20)) . (map read) . concat . map (splitOn " ") . lines $ a
  return . maximum $ mkProducts arr 4

mkVector :: Int -> Dir -> Coord -> [Coord]
mkVector len dir coord = take len $ iterate (apply dir) coord
  where apply (Dir fx fy) (x,y) = (fx x,fy y)

mkProducts :: Matrix -> Int -> [Int]
mkProducts arr size = do
  index <- indices arr
  direction <- [Dir (+1) id, Dir id (+1), Dir (+1) (+1), Dir (+1) (subtract 1)]
  let combos = [(x,y) | (x,y) <- mkVector size direction index]
  guard $ all (inRange (bounds arr)) combos
  return $ foldl1 (*) (map (arr !) combos)
