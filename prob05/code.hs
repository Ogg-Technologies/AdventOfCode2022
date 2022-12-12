import           Data.List.Split
import           Data.List
import           Data.Char

parseStacks :: String -> [String]
parseStacks str = d
 where
  a = lines str
  b = transpose a
  c = map fst $ filter (\(_, i) -> i `mod` 4 == 1) (zip b [0 ..])
  d = map (filter isUpper) c

parseMoves :: String -> [(Int, Int, Int)]
parseMoves str = map
  (\ws -> (read $ ws !! 1, read (ws !! 3) - 1, read (ws !! 5) - 1))
  l
  where l = map words $ lines str

replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

move :: [String] -> (Int, Int, Int) -> [String]
move stacks (0, from, to) = stacks
move stacks (n, from, to) = move newStack (n - 1, from, to)
 where
  e        = head (stacks !! from)
  newFrom  = tail (stacks !! from)
  newTo    = e : (stacks !! to)
  newStack = replace to newTo $ replace from newFrom stacks


move2 :: [String] -> (Int, Int, Int) -> [String]
move2 stacks (n, from, to) = newStack
 where
  es       = take n (stacks !! from)
  newFrom  = drop n (stacks !! from)
  newTo    = es ++ (stacks !! to)
  newStack = replace to newTo $ replace from newFrom stacks

main :: IO ()
main = do
  [l1, l2] <- splitOn "\n\n" <$> readFile "prob05/input.txt"
  let stacks = parseStacks l1
  let moves  = parseMoves l2
  let moved  = foldl move2 stacks moves
  print $ map head moved

