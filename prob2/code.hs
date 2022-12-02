import           Data.List.Split
import           Data.List
import           Data.Ord

scoreFor :: String -> Int

scoreFor "A X" = 3 + 1
scoreFor "B Y" = 3 + 2
scoreFor "C Z" = 3 + 3

scoreFor "A Y" = 6 + 2
scoreFor "B Z" = 6 + 3
scoreFor "C X" = 6 + 1

scoreFor "A Z" = 0 + 3
scoreFor "B X" = 0 + 1
scoreFor "C Y" = 0 + 2



scoreFor2 :: String -> Int

scoreFor2 "A X" = 0 + 3
scoreFor2 "B Y" = 3 + 2
scoreFor2 "C Z" = 6 + 1

scoreFor2 "A Y" = 3 + 1
scoreFor2 "B Z" = 6 + 3
scoreFor2 "C X" = 0 + 2

scoreFor2 "A Z" = 6 + 2
scoreFor2 "B X" = 0 + 1
scoreFor2 "C Y" = 3 + 3

main :: IO ()
main = do
  content <- readFile "prob2/input.txt"
  let strat  = lines content
  let scores = map scoreFor2 strat
  print (sum scores)
