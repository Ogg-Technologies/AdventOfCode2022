import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Ord


parseStarting :: String -> [Int]
parseStarting s = map (read . filter isDigit) (drop 2 $ words (lines s !! 1))

parseOp :: String -> Int -> Int
parseOp s old = old `op` other
 where
  ws    = drop 3 $ words (lines s !! 2)
  op    = if (ws !! 1) == "*" then (*) else (+)
  other = if (ws !! 2) == "old" then old else read (ws !! 2)

parseTest :: String -> Int -> Int
parseTest s n = if n `mod` divN == 0 then tCase else fCase
 where
  divN  = read $ last $ words (lines s !! 3)
  tCase = read $ last $ words (lines s !! 4)
  fCase = read $ last $ words (lines s !! 5)

parseModVal :: String -> Int
parseModVal s = read $ last $ words (lines s !! 3)

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (i, x) = take i xs ++ [x] ++ drop (i + 1) xs

doActions
  :: ([[Int]], [Int], [Int -> Int], [Int -> Int], Int)
  -> Int
  -> ([[Int]], [Int], [Int -> Int], [Int -> Int], Int)
doActions (curr, totInspections, ops, tests, modVal) i
  | null (curr !! i) = (curr, totInspections, ops, tests, modVal)
  | otherwise        = doActions (newCurr, newTot, ops, tests, modVal) i
 where
  op         = ops !! i
  test       = tests !! i
  oldN       = head (curr !! i)
  newN       = op oldN `mod` modVal
  nextIndexN = test newN
  newTot     = totInspections !!= (i, totInspections !! i + 1)
  newCurr =
    curr
      !!= (i         , tail (curr !! i))
      !!= (nextIndexN, (curr !! nextIndexN) ++ [newN])

doRound
  :: ([[Int]], [Int], [Int -> Int], [Int -> Int], Int)
  -> ([[Int]], [Int], [Int -> Int], [Int -> Int], Int)
doRound (c, i, o, t, m) = foldl doActions (c, i, o, t, m) [0 .. length c - 1]

main :: IO ()
main = do
  l <- splitOn "\n\n" <$> readFile "prob11/input.txt"
  let starting = map parseStarting l
  let ops      = map parseOp l
  let test     = map parseTest l
  let modVal   = product $ map parseModVal l
  print modVal
  let (curr, totInspections, _, _, _) = foldl
        (\acc i -> doRound acc)
        (starting, replicate (length starting) 0, ops, test, modVal)
        [1 .. 10000]
  print curr
  print totInspections
  let sortedIs = sortOn Down totInspections
  print $ head sortedIs * sortedIs !! 1
