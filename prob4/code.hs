import           Data.List
import           Data.List.Split
import           Data.Char

parseRow :: String -> (Int, Int, Int, Int)
parseRow s = (l1, h1, l2, h2)
  where [l1, h1, l2, h2] = map read $ concatMap (splitOn "-") $ splitOn "," s

withinCount :: (Int, Int, Int, Int) -> Int
withinCount (l1, h1, l2, h2) | l1 <= l2 && h1 >= h2 = 1
                             | l2 <= l1 && h2 >= h1 = 1
                             | otherwise            = 0

overlapCount :: (Int, Int, Int, Int) -> Int
overlapCount (l1, h1, l2, h2) | l1 <= h2 && h1 >= l2 = 1
                              | otherwise            = 0


main :: IO ()
main = do
  l <- map parseRow . lines <$> readFile "prob4/input.txt"
  print $ sum $ map withinCount l
  print $ sum $ map overlapCount l
