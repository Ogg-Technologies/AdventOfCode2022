import           Data.List.Split
import           Data.List
import           Data.Ord
import           Data.Maybe

outcomeScore :: (Int, Int) -> Int
outcomeScore (o, m) = [0, 3, 6] !! ((1 + m - o) `mod` 3)

scoreFor :: (Int, Int) -> Int
scoreFor (o, m) = (m + 1) + outcomeScore (o, m)

toNums :: String -> (Int, Int)
toNums s =
  (fromJust (elemIndex (head s) "ABC"), fromJust (elemIndex (last s) "XYZ"))
  where w = words s

fromOutcome :: (Int, Int) -> (Int, Int)
fromOutcome (o, 0) = (o, (o - 1) `mod` 3)
fromOutcome (o, 1) = (o, o)
fromOutcome (o, 2) = (o, (o + 1) `mod` 3)

main :: IO ()
main = do
  content <- readFile "prob02/input.txt"
  let strat = map toNums $ lines content
  print $ sum $ map scoreFor strat
  print $ sum $ map (scoreFor . fromOutcome) strat
