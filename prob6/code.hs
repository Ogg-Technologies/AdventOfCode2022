import           Data.List.Split
import           Data.List
import           Data.Char
import           Data.Maybe

windowed :: Int -> Int -> String -> [String]
windowed size step str
  | length str <= size = [str]
  | otherwise          = take size str : windowed size step (drop step str)


main :: IO ()
main = do
  l <- readFile "prob6/input.txt"
  let w1 = windowed 4 1 l
  print $ fromJust (findIndex (\s -> nub s == s) w1) + length (head w1)
  let w2 = windowed 14 1 l
  print $ fromJust (findIndex (\s -> nub s == s) w2) + length (head w2)

