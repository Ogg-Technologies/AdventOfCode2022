import           Data.List.Split
import           Data.List
import           Data.Maybe

data Cmd = Cd String | Ls [String]
  deriving Show

toCmd :: [String] -> Cmd
toCmd (c : rs) | c == "ls"        = Ls rs
               | take 2 c == "cd" = Cd (drop 3 c)

rsToFiles :: String -> [String] -> [(Int, String)]
rsToFiles path rs = map
  (\r -> (read (head (words r)), path ++ "/" ++ last (words r)))
  fileStrings
  where fileStrings = filter (\r -> head (words r) /= "dir") rs

newPath :: String -> String -> String
newPath path ".." = take (last (elemIndices '/' path)) path
newPath path s    = path ++ "/" ++ s

-- trav files path cmd -> newFiles
trav :: ([(Int, String)], String) -> [Cmd] -> [(Int, String)]
trav (files, path) []              = files
trav (files, path) ((Cd s) : cmds) = trav (files, newPath path s) cmds
trav (files, path) ((Ls rs) : cmds) =
  trav (files ++ rsToFiles path rs, path) cmds

getDirectories :: [(Int, String)] -> [String]
getDirectories files = nub $ do
  f <- fileNames
  let slashes = elemIndices '/' f
  let ds      = map (\i -> take i f ++ "/") slashes
  ds
  where fileNames = map snd files

getDirSize :: String -> [(Int, String)] -> Int
getDirSize dir files =
  sum $ map fst $ filter (\(s, f) -> dir `isPrefixOf` f) files

main :: IO ()
main = do
  l <- readFile "prob7/input.txt"
  let a        = drop 1 $ splitOn "\n$ " l
  let b        = map lines a
  let cmds     = map toCmd b
  let allFiles = trav ([], "") cmds
  let dirSizes =
        map (\d -> (d, getDirSize d allFiles)) (getDirectories allFiles)
  print $ sum $ map snd $ filter (\(d, s) -> s < 100000) dirSizes


  let usedSpace     = sum $ map fst allFiles
  let capacity      = 70000000
  let requiredSpace = 30000000
  let toDelete      = usedSpace - (capacity - requiredSpace)
  print toDelete
  print $ minimum $ filter (> toDelete) $ map snd dirSizes
