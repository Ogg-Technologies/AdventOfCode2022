import           Data.List
import           Data.List.Split
import           Data.Char

prio :: Char -> Int
prio c = ord c + (if isUpper c then 27 - ord 'A' else 1 - ord 'a')

splitMiddle :: String -> [String]
splitMiddle s = chunksOf (length s `div` 2) s

getPrioSum :: [[String]] -> Int
getPrioSum elems = sum $ map (prio . head . foldr1 intersect) elems

main :: IO ()
main = do
  l <- lines <$> readFile "prob03/input.txt"
  print $ getPrioSum $ map splitMiddle l
  print $ getPrioSum $ chunksOf 3 l
