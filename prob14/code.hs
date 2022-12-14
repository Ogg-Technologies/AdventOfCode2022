import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Ord

parseLinePositions :: String -> [(Int, Int)]
parseLinePositions s = map (\[x, y] -> (read x, read y)) coords
 where
  posStrings = splitOn " -> " s
  coords     = map (splitOn ",") posStrings

clamp :: Int -> Int -> Int -> Int
clamp x lo hi | x > hi    = hi
              | x < lo    = lo
              | otherwise = x

lineToPositions :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineToPositions (x1, y1) (x2, y2) = map
  (\i -> (x1 + xStep * i, y1 + yStep * i))
  [0 .. steps]
 where
  xStep = clamp (x2 - x1) (-1) 1
  yStep = clamp (y2 - y1) (-1) 1
  steps = max (abs (x2 - x1)) (abs (y2 - y1))

allRockPositions :: [[(Int, Int)]] -> [(Int, Int)]
allRockPositions = concatMap
  (\line -> concatMap (uncurry lineToPositions) (zip line (tail line)))

data Tile = Empty | Rock | Sand deriving (Show, Eq)

rockPositionsToMap :: [(Int, Int)] -> [[Tile]]
rockPositionsToMap ps = map
  (\y -> map (\x -> if (x, y) `elem` ps then Rock else Empty) [0 .. maxX])
  [0 .. maxY]
 where
  maxX = maximum $ map fst ps
  maxY = maximum $ map snd ps

printMap :: [[Tile]] -> IO ()
printMap m = putStrLn $ unlines $ map
  (map (\x -> if x == Rock then '#' else if x == Sand then 'o' else '.'))
  m

mapAt :: [[Tile]] -> (Int, Int) -> Tile
mapAt m (x, y) | y < 0 || y >= length m        = Empty
               | x < 0 || x >= length (m !! y) = Empty
               | otherwise                     = m !! y !! x

moveSand :: [[Tile]] -> (Int, Int) -> (Int, Int)
moveSand m (x, y) | d == Empty  = (x, y + 1)
                  | dl == Empty = (x - 1, y + 1)
                  | dr == Empty = (x + 1, y + 1)
                  | otherwise   = (x, y)
 where
  d  = mapAt m (x, y + 1)
  dl = mapAt m (x - 1, y + 1)
  dr = mapAt m (x + 1, y + 1)

setMapAt :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
setMapAt m (x, y) t =
  take y m
    ++ [take x (m !! y) ++ [t] ++ drop (x + 1) (m !! y)]
    ++ drop (y + 1) m

isWithinMap :: [[Tile]] -> (Int, Int) -> Bool
isWithinMap m (x, y) = y >= 0 && y < length m && x >= 0 && x < length (m !! y)

dropSand :: [[Tile]] -> (Int, Int) -> Maybe [[Tile]]
dropSand m pos | not $ isWithinMap m pos = Nothing
               | mapAt m pos == Sand     = Nothing
               | pos == npos             = Just $ setMapAt m pos Sand
               | otherwise               = dropSand m npos
  where npos = moveSand m pos

dropSands :: [[Tile]] -> (Int, Int) -> [[Tile]]
dropSands m pos | isNothing m' = m
                | otherwise    = dropSands (fromJust m') pos
  where m' = dropSand m pos

countSands :: [[Tile]] -> Int
countSands m = length $ filter (== Sand) $ concat m

addInfiniteFloor :: [[(Int, Int)]] -> [[(Int, Int)]]
addInfiniteFloor ps = ps ++ [[(left, maxY + 2), (right, maxY + 2)]]
 where
  maxY      = maximum $ map (maximum . map snd) ps
  maxX      = maximum $ map (maximum . map fst) ps
  minX      = minimum $ map (minimum . map fst) ps
  center    = (minX + maxX) `div` 2
  halfWidth = maxY + 30
  left      = center - halfWidth
  right     = center + halfWidth


main :: IO ()
main = do
  l <- lines <$> readFile "input.txt"
  let originalPs = addInfiniteFloor $ map parseLinePositions l
  let minX       = minimum $ map (minimum . map fst) originalPs
  let ps         = map (map (\(x, y) -> (x - minX, y))) originalPs
  let m          = rockPositionsToMap $ allRockPositions ps
  --printMap m
  --printMap $ fromJust $ dropSand m (500 - minX, 0)
  let sandMap    = dropSands m (500 - minX, 0)
  --printMap sandMap
  print $ countSands sandMap

