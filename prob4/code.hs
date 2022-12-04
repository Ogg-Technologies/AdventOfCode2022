import           Data.List
import           Data.List.Split
import           Data.Char

parseRow :: String -> (Int, Int, Int, Int)
parseRow s = (a, b, c, d)
  where [a, b, c, d] = map read $ concatMap (splitOn "-") $ splitOn "," s

withinCount :: (Int, Int, Int, Int) -> Int
withinCount (a, b, c, d) | a <= c && b >= d = 1
                         | c <= a && d >= b = 1
                         | otherwise        = 0

overlapCount :: (Int, Int, Int, Int) -> Int
overlapCount (a, b, c, d) | a <= d && b >= c = 1
                          | otherwise        = 0


main :: IO ()
main = do
  l <- lines <$> readFile "prob4/input.txt"
  print $ sum $ map (withinCount . parseRow) l
  print $ sum $ map (overlapCount . parseRow) l
