import           Data.List.Split
import           Data.List
import           Data.Ord
import           Data.Maybe

outcomeScore :: (Int, Int) -> Int
outcomeScore (o, m) = [0, 3, 6] !! ((1 + m - o) `mod` 3)

scoreFor :: (Int, Int) -> Int
scoreFor (o, m) = outcomeScore (o, m) + m

toNums :: String -> (Int, Int)
toNums s =
  ( fromJust (elemIndex (head s) "ABC") + 1
  , fromJust (elemIndex (last s) "XYZ") + 1
  )
  where w = words s

fromOutcome :: (Int, Int) -> (Int, Int)
fromOutcome (o, 1) = (o, (o - 2) `mod` 3 + 1)
fromOutcome (o, 2) = (o, o)
fromOutcome (o, 3) = (o, o `mod` 3 + 1)

main :: IO ()
main = do
  content <- readFile "prob2/input.txt"
  let strat = lines content
  print (sum (map (scoreFor . toNums) strat))
  print (sum (map (scoreFor . fromOutcome . toNums) strat))
