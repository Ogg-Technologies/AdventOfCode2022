import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Ord


parseStarting :: String -> [Integer]
parseStarting s = map (read . filter isDigit) (drop 2 $ words (lines s !! 1))

parseOp :: String -> Integer -> Integer
parseOp s old = old `op` other
 where
  ws    = drop 3 $ words (lines s !! 2)
  op    = if (ws !! 1) == "*" then (*) else (+)
  other = if (ws !! 2) == "old" then old else read (ws !! 2)

parseTest :: String -> Integer -> Int
parseTest s n = if n `mod` divN == 0 then tCase else fCase
 where
  divN  = read $ last $ words (lines s !! 3)
  tCase = read $ last $ words (lines s !! 4)
  fCase = read $ last $ words (lines s !! 5)

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (i, x) = take i xs ++ [x] ++ drop (i + 1) xs

doActions
  :: ([[Integer]], [Int], [Integer -> Integer], [Integer -> Int])
  -> Int
  -> ([[Integer]], [Int], [Integer -> Integer], [Integer -> Int])
doActions (curr, totInspections, ops, tests) i
  | null (curr !! i) = (curr, totInspections, ops, tests)
  | otherwise        = doActions (newCurr, newTot, ops, tests) i
 where
  op         = ops !! i
  test       = tests !! i
  oldN       = head (curr !! i)
  newN       = op oldN -- `div` 3
  nextIndexN = test newN
  newTot     = totInspections !!= (i, totInspections !! i + 1)
  newCurr =
    curr
      !!= (i         , tail (curr !! i))
      !!= (nextIndexN, (curr !! nextIndexN) ++ [newN])

doRound
  :: ([[Integer]], [Int], [Integer -> Integer], [Integer -> Int])
  -> ([[Integer]], [Int], [Integer -> Integer], [Integer -> Int])
doRound (c, i, o, t) = foldl doActions (c, i, o, t) [0 .. length c - 1]

main :: IO ()
main = do
  l <- splitOn "\n\n" <$> readFile "prob11/mini_input.txt"
  let starting = map parseStarting l
  let ops      = map parseOp l
  let test     = map parseTest l
  let (curr, totInspections, _, _) = foldl
        (\acc i -> doRound acc)
        (starting, replicate (length starting) 0, ops, test)
        [1 .. 10000]
  print curr
  print totInspections
  let sortedIs = sortOn Down totInspections
  print $ head sortedIs * sortedIs !! 1

-- 46316663904
