import           Data.List.Split
import           Data.List
import           Data.Maybe


parseToTrees :: [String] -> [[Int]]
parseToTrees = map (map (\c -> read [c]))

isVisible :: [[Int]] -> Int -> Int -> Bool
isVisible trees 0 y = True
isVisible trees x 0 = True
isVisible trees x y | y == length trees - 1        = True
                    | x == length (trees !! y) - 1 = True
isVisible trees x y =
  (t > maximum ls) || (t > maximum rs) || (t > maximum us) || (t > maximum ds)
 where
  t  = trees !! y !! x
  ls = take x (trees !! y)
  rs = drop (x + 1) (trees !! y)
  us = take y (transpose trees !! x)
  ds = drop (y + 1) (transpose trees !! x)

seeingLength :: [Int] -> Int -> Int
seeingLength treeLine t | isJust i  = fromJust i + 1
                        | otherwise = length treeLine
  where i = findIndex (>= t) treeLine

scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore trees x y =
  seeingLength ls t * seeingLength rs t * seeingLength us t * seeingLength ds t
 where
  t  = trees !! y !! x
  ls = reverse $ take x (trees !! y)
  rs = drop (x + 1) (trees !! y)
  us = reverse $ take y (transpose trees !! x)
  ds = drop (y + 1) (transpose trees !! x)

main :: IO ()
main = do
  l <- lines <$> readFile "prob8/input.txt"
  let trees = parseToTrees l
  let positions =
        [ (x, y)
        | x <- [0 .. length (head trees) - 1]
        , y <- [0 .. length trees - 1]
        ]
  let visiblePositions = filter (uncurry (isVisible trees)) positions
  print $ length visiblePositions

  let scores = map (uncurry (scenicScore trees)) visiblePositions
  print $ maximum scores
