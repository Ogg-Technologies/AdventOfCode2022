import           Data.List
import           Data.Maybe

prioritiesList = "-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
prio :: Char -> Int
prio c = fromJust (elemIndex c prioritiesList)

tripples :: [String] -> [[String]]
tripples = tripples_ []

tripples_ :: [[String]] -> [String] -> [[String]]
tripples_ acc []               = acc
tripples_ acc (x : y : z : xs) = tripples_ (acc ++ [[x, y, z]]) xs

splitMiddle :: String -> [String]
splitMiddle s = [x, y] where (x, y) = splitAt (length s `div` 2) s

intersectList :: [String] -> String
intersectList (x : xs) = foldl intersect x xs

getPrioSum :: [[String]] -> Int
getPrioSum elems = sum $ map (prio . head . intersectList) elems

main :: IO ()
main = do
  l <- lines <$> readFile "prob3/input.txt"
  print $ getPrioSum $ map splitMiddle l
  print $ getPrioSum $ tripples l
