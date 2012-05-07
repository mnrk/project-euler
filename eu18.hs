import Data.List.Split
import Data.List

newtype Tree = Tree {runTree :: [[Int]]}
data Pos = Pos {row :: Int, offset :: Int}
type Pick = (Tree -> Pos -> Pos)

(!) :: Tree -> Pos -> Int
(!) (Tree tree) (Pos depth offset) = tree !! (depth - 1) !! (offset - 1)

depth :: Tree -> Int
depth (Tree tree) = length tree

childs :: Pos -> (Pos,Pos)
childs (Pos a b) = (Pos (a+1) b, Pos (a+1) (b+1))

createTree :: String -> Tree
createTree content = Tree $ map (map readInt . (splitOn " ")) $ lines content
  where readInt = read :: String -> Int

highestChild :: Pick
highestChild tree pos = if lVal < rVal then rPos else lPos
  where (lPos,rPos) = childs pos
        (lVal,rVal) = (tree ! lPos, tree ! rPos)

walk :: Pick -> Tree -> Pos -> Int -> Int
walk pick tree start steps
  | steps == 0 = value
  | otherwise  = value + walk pick tree (pick tree start) (steps-1)
    where value = tree ! start

eu18 = do
  c <- readFile "data/eu18.txt"
  let tree = createTree c
  return $ walk highestChild tree (Pos 1 1) (depth tree - 1)
