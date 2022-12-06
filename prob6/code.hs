import           Data.List.Split
import           Data.List
import           Data.Char
import           Data.Maybe

windowed :: Int -> Int -> String -> [String]
windowed size step str
  | length str <= size = [str]
  | otherwise          = [take size str] ++ (windowed size step (drop step str))


main :: IO ()
main = do
  l <- readFile "prob6/input.txt"
  let a = windowed 14 1 l
  let b = fromJust (findIndex (\s -> nub s == s) a) + 14
  print $ b

