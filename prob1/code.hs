import           Data.List.Split
import           Data.List
import           Data.Ord

getTotals :: String -> [Int]
getTotals = map (sum . map read . lines) . splitOn "\n\n"

main :: IO ()
main = do
  elfTotals <- getTotals <$> readFile "prob1/input.txt"
  let sortedElfTotals = sortOn Down elfTotals
  print $ head sortedElfTotals
  print $ sum $ take 3 sortedElfTotals


