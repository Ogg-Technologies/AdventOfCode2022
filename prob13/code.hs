import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Ord
import           Parsing

data Signal = I Int | L [Signal]
  deriving (Show, Eq, Read)

signalParser :: Parser Signal
signalParser = (I <$> readsP) <|> do
  char '['
  s <- chain signalParser (char ',') <|> return []
  char ']'
  return $ L s

parseSignal :: String -> Signal
parseSignal = fst . fromJust . parse signalParser

parseSignalGroup :: String -> (Signal, Signal)
parseSignalGroup s = (a, b) where [a, b] = map parseSignal (lines s)


-- returns negative number if first is less than second
-- returns positive number if first is greater than second
-- returns 0 if they are equal
cmp :: Signal -> Signal -> Int
cmp (I a) (I b) = a - b
cmp (L (a : as)) (L (b : bs)) | c == 0    = cmp (L as) (L bs)
                              | otherwise = c
  where c = cmp a b
cmp (L []) (L []) = 0
cmp (L []) (L _ ) = -1
cmp (L _ ) (L []) = 1
cmp (I a ) (L bs) = cmp (L [I a]) (L bs)
cmp (L as) (I b ) = cmp (L as) (L [I b])

instance Ord Signal where
  compare a b = compare (cmp a b) 0

main :: IO ()
main = do
  l <- splitOn "\n\n" <$> readFile "input.txt"
  let signals = map parseSignalGroup l
  let cmps    = map (uncurry cmp) signals
  let indices = map snd (filter (\(c, i) -> c < 0) (cmps `zip` [1 ..]))
  print $ sum indices

  let [d2, d6]    = map parseSignal ["[[2]]", "[[6]]"]
  let linesList   = concatMap lines l
  let signalsList = map parseSignal linesList ++ [d2, d6]
  let sorted      = sort signalsList
  let i2          = fromJust (elemIndex d2 sorted) + 1
  let i6          = fromJust (elemIndex d6 sorted) + 1
  print $ i2 * i6
