import           Data.List.Split

parseRow :: String -> (Int, Int, Int, Int)
parseRow s = (l1, h1, l2, h2)
  where [l1, h1, l2, h2] = map read $ concatMap (splitOn "-") $ splitOn "," s

within :: (Int, Int, Int, Int) -> Bool
within (l1, h1, l2, h2) = l1 <= l2 && h1 >= h2 || l2 <= l1 && h2 >= h1

overlaps :: (Int, Int, Int, Int) -> Bool
overlaps (l1, h1, l2, h2) = l1 <= h2 && h1 >= l2

main :: IO ()
main = do
  l <- map parseRow . lines <$> readFile "prob4/input.txt"
  print $ length $ filter within l
  print $ length $ filter overlaps l
