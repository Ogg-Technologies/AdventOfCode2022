import           Data.List.Split
import           Data.List
import           Data.Ord
import           Data.Maybe


prio = "-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

tripples :: [String] -> [(String, String, String)]
tripples = tripples_ []

tripples_
  :: [(String, String, String)] -> [String] -> [(String, String, String)]
tripples_ acc []               = acc
tripples_ acc (x : y : z : xs) = tripples_ (acc ++ [(x, y, z)]) xs

main :: IO ()
main = do
  content <- readFile "prob3/input.txt"
  let a = lines content
  let b = tripples a
  print b
  let c = map (\(x, y, z) -> head $ intersect (x `intersect` y) z) b
  let d = map (\x -> fromJust (elemIndex x prio)) c
  print $ sum d

  --content <- readFile "prob3/input.txt"
  --let a = lines content
  --let b = map (\l -> splitAt (length l `div` 2) l) a
  --let c = map (uncurry intersect) b
  --let d = map head c
  --let e = map (\x -> fromJust (elemIndex x prio)) d
  --print $ sum e
