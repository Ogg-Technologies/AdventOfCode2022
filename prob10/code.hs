import           Data.List.Split
import           Data.List
import           Data.Maybe

parseUpdate :: String -> [Maybe Int]
parseUpdate "noop" = [Nothing]
parseUpdate s      = [Nothing, Just $ read n] where [_, n] = words s

getStates :: [Maybe Int] -> [Int]
getStates = foldl
  (\acc x -> case x of
    Nothing -> acc ++ [last acc]
    Just n  -> acc ++ [last acc + n]
  )
  [1]

everynth :: Int -> [a] -> [a]
everynth n l = map fst $ filter ((== 0) . (`mod` n) . snd) $ zip l [0 ..]

getRow :: [(Int, Int)] -> String
getRow = map (\(wp, dp) -> if abs (wp - dp) <= 1 then '#' else ' ')

main :: IO ()
main = do
  l <- lines <$> readFile "prob10/input.txt"
  let updates        = concatMap parseUpdate l
  let states         = getStates updates
  let relevantStates = everynth 40 (drop 19 states)
  let prods = zipWith (*) relevantStates [ 20 + i * 40 | i <- [0 ..] ]
  print $ sum prods

  let groupedStates = map (zip [0 ..]) (chunksOf 40 states)
  putStrLn $ unlines $ map getRow groupedStates
