import qualified Data.PQueue.Prio.Min          as PQ
import qualified Data.HashSet                  as Set
import qualified Data.HashMap.Strict           as Map
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Ord

getElevation :: Char -> Int
getElevation 'S'           = getElevation 'a'
getElevation 'E'           = getElevation 'z'
getElevation c | isLower c = ord c - ord 'a'

getElevations :: [String] -> [[Int]]
getElevations = map (map getElevation)

find2d :: (a -> Bool) -> [[a]] -> Maybe (Int, Int)
find2d f m = find2d' f m 0 where
  find2d' _ []       _ = Nothing
  find2d' f (x : xs) i = case findIndex f x of
    Nothing -> find2d' f xs (i + 1)
    Just j  -> Just (j, i)


nextValid :: [[Int]] -> (Int, Int) -> [(Int, Int)]
nextValid es (x, y) = filter (\(x', y') -> (es !! y') !! x' <= current + 1)
                             boundedMoves
 where
  current      = (es !! y) !! x
  boundedMoves = filter
    (\(x', y') -> x' >= 0 && x' < length (head es) && y' >= 0 && y' < length es)
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

dfs :: [[Int]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Maybe Int
dfs es start end visited
  | start == end = Just 0
  | null nexts = Nothing
  | otherwise = case catMaybes dists of
    [] -> Nothing
    xs -> Just $ minimum xs + 1
 where
  nexts = filter (`notElem` visited) $ nextValid es start
  dists = map (\s -> dfs es s end (s : visited)) nexts

distanceHeuristic :: (Int, Int) -> (Int, Int) -> Int
distanceHeuristic (x, y) (x', y') = abs (x - x') + abs (y - y')

-- Heavily inspired by https://gist.github.com/abhin4v/8172534

astarSearch
  :: [[Int]] -- Elevation map
  -> (Int, Int) -- Start
  -> (Int, Int) -- Goal
  -> Maybe (Int, [(Int, Int)]) -- resulting distance and path
astarSearch es start end = astar
  (PQ.singleton (distanceHeuristic start end) (start, 0))
  Set.empty
  (Map.singleton start 0)
  Map.empty
 where
  astar pq seen gscore tracks
    | PQ.null pq           = Nothing
    | node == end          = Just (gcost, findPath tracks node)
    | Set.member node seen = astar pq' seen gscore tracks
    | otherwise            = astar pq'' seen' gscore' tracks'
   where
    (node, gcost) = snd . PQ.findMin $ pq
    pq'           = PQ.deleteMin pq
    seen'         = Set.insert node seen
    successors =
      filter
          (\(s, g, _) ->
            not (Set.member s seen')
              && (  not (s `Map.member` gscore)
                 || g
                 <  (fromJust . Map.lookup s $ gscore)
                 )
          )
        $ successorsAndCosts node gcost
    pq''    = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
    gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
    tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
  successorsAndCosts node gcost =
    map (\s -> (s, gcost + 1, distanceHeuristic s end)) . nextValid es $ node

  -- Constructs the path from the tracks and last node
  findPath tracks node = if Map.member node tracks
    then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
    else [node]

all0Positions :: [[Int]] -> [(Int, Int)]
all0Positions es =
  [ (x, y)
  | y <- [0 .. length es - 1]
  , x <- [0 .. length (head es) - 1]
  , (es !! y) !! x == 0
  ]

main :: IO ()
main = do
  inputString <- readFile "prob12/input.txt"
  let ls    = lines inputString
  let es    = getElevations ls
  let start = fromJust $ find2d (== 'S') ls
  let end   = fromJust $ find2d (== 'E') ls
  print es
  print start
  print end
  let allDistances = map (\s -> astarSearch es s end) $ all0Positions es
  print $ minimum $ catMaybes allDistances
