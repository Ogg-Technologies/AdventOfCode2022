import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Ord

parseNumbers :: String -> (Int, Int, Int, Int)
parseNumbers s = (sx, sy, bx, by)
 where
  w  = words s
  sx = read $ filter isDigit $ w !! 2
  sy = read $ filter isDigit $ w !! 3
  bx = read $ filter isDigit $ w !! 8
  by = read $ filter isDigit $ w !! 9

isBeacon :: [(Int, Int, Int, Int)] -> (Int, Int) -> Bool
isBeacon sensors (x, y) = any (\(sx, sy, bx, by) -> x == bx && y == by) sensors

possibleBeaconByDist :: [(Int, Int, Int, Int)] -> (Int, Int) -> Bool
possibleBeaconByDist sensors (x, y) = all
  (\(sx, sy, bx, by) ->
    abs (x - sx) + abs (y - sy) > abs (bx - sx) + abs (by - sy)
  )
  sensors

impossibleBeacon :: [(Int, Int, Int, Int)] -> (Int, Int) -> Bool
impossibleBeacon sensors pos@(x, y) =
  not (isBeacon sensors pos) && not (possibleBeaconByDist sensors pos)

main :: IO ()
main = do
  l <- lines <$> readFile "input.txt"
  let sensors = map parseNumbers l
  let y       = 2000000
  let xRange  = 100000000
  print $ length $ filter (impossibleBeacon sensors)
                          [ (x, y) | x <- [-xRange .. xRange] ]

