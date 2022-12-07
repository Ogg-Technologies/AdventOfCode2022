import           Data.List.Split
import           Data.List
import           Data.Maybe


{-
-- Filesystem is
-- a directory with name and other filesystems
-- or a file with a size and name
data FS = Dir String [FS] | File Int String
  deriving Show

data Cmd = Cd String | Ls [String]
  deriving Show

toCmd :: [String] -> Cmd
toCmd (c : rs) | c == "ls"        = Ls rs
               | take 2 c == "cd" = Cd (drop 3 c)


rToFS :: String -> FS
rToFS r | w1 == "dir" = Dir w2 []
        | otherwise   = File (read w1) w2
  where [w1, w2] = words r

writeToPath :: FS -> [String] -> [String] -> FS
writeToPath (Dir _ l) [p] rs = Dir $ take i l ++ [Dir p []] ++ drop (i + 1) l
 where
  i      = findIndex (\(Dir n _) -> n == p) l
  newDir = Dir p (map rToFS rs)


traverse :: (FS, [String]) -> Cmd -> (FS, [String])
traverse (fs, path) (Cd s ) = (fs, path ++ "/" ++ s)
traverse (fs, path) (Ls rs) = (fs, path)
-}

data Cmd = Cd String | Ls [String]
  deriving Show

toCmd :: [String] -> Cmd
toCmd (c : rs) | c == "ls"        = Ls rs
               | take 2 c == "cd" = Cd (drop 3 c)

rsToFiles :: [String] -> [(Int, String)]
rsToFiles rs = map (\r -> (read (head (words r)), last (words r))) fileStrings
  where fileStrings = filter (\r -> head (words r) /= "dir") rs


-- trav files path cmd -> newFiles
trav :: ([(Int, String)], String) -> [Cmd] -> [(Int, String)]
trav (files, path) []               = files
trav (files, path) ((Cd s ) : cmds) = trav (files, (path ++ "/" ++ s)) cmds
trav (files, path) ((Ls rs) : cmds) = trav ((files ++ rsToFiles rs), path) cmds

main :: IO ()
main = do
  l <- readFile "prob7/mini_input.txt"
  let a = drop 1 $ splitOn "\n$ " l
  let b = map lines a
  let c = map toCmd b
  print c


  print $ trav ([], "") [Cd "a"]

  print $ trav ([], "") c
