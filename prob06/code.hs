import           Data.List.Split
import           Data.List
import           Data.Maybe

firstUnique :: [String] -> Int
firstUnique = fromJust . findIndex (\s -> nub s == s)

findMarker :: Int -> String -> Int
findMarker n s = firstUnique (divvy n 1 s) + n

main :: IO ()
main = do
  l <- readFile "prob06/input.txt"
  print $ findMarker 4 l
  print $ findMarker 14 l

