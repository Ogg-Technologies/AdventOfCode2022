import           Data.List.Split
import           Data.List
import           Data.Maybe

clamp :: Int -> Int -> Int -> Int
clamp x lo hi | x > hi    = hi
              | x < lo    = lo
              | otherwise = x

data Dir = U | D | L | R deriving (Show, Eq, Read)

moveHead :: Dir -> (Int, Int) -> (Int, Int)
moveHead U (x, y) = (x, y - 1)
moveHead D (x, y) = (x, y + 1)
moveHead L (x, y) = (x - 1, y)
moveHead R (x, y) = (x + 1, y)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty)
  | (abs (hx - tx) <= 1) && (abs (hy - ty) <= 1) = (tx, ty)
  | otherwise = (tx + mx, ty + my)
 where
  mx = clamp (hx - tx) (-1) 1
  my = clamp (hy - ty) (-1) 1

parseMove :: String -> [Dir]
parseMove l = replicate (read n) (read d) where [d, n] = words l

runMoves :: [(Int, Int)] -> [Dir] -> [(Int, Int)]
runMoves ps []       = [last ps]
runMoves ps (d : ds) = last ps : runMoves ps' ds
 where
  ps' :: [(Int, Int)]
  ps' = foldl
    (\acc p -> case acc of
      [] -> [moveHead d p]
      _  -> acc ++ [moveTail (last acc) p]
    )
    []
    ps

main :: IO ()
main = do
  moves <- concatMap parseMove . lines <$> readFile "prob09/input.txt"
  let visited = runMoves (replicate 2 (0, 0)) moves
  print $ length $ nub visited
  let visited2 = runMoves (replicate 10 (0, 0)) moves
  print $ length $ nub visited2
