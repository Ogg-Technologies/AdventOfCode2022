import           Data.List.Split
import           Data.List
import           Data.Ord

part1 :: IO ()
part1 = do
  str <- readFile "prob1/input.txt"
  let elfSplit  = splitOn "\n\n" str
  let lineSplit = map lines elfSplit
  let elfNums   = map (\l -> map read l :: [Int]) lineSplit
  let elfTotals = map sum elfNums
  let maxTotal  = maximum elfTotals
  print maxTotal

part2 :: IO ()
part2 = do
  str <- readFile "prob1/input.txt"
  let elfSplit        = splitOn "\n\n" str
  let lineSplit       = map lines elfSplit
  let elfNums         = map (\l -> map read l :: [Int]) lineSplit
  let elfTotals       = map sum elfNums
  let sortedElfTotals = sortOn Down elfTotals
  let maxTotal3       = sum (take 3 sortedElfTotals)
  print maxTotal3


main :: IO ()
main = part2
