import MyGraph
import System.IO
import System.Environment
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> solve stdin
    file:_ -> withFile file ReadMode solve
  
solve :: Handle -> IO ()
solve handle = do
  contents <- hGetContents handle
  let graphData = map parseLine $ lines contents
  putStr . unwords . map show $ calculateResult graphData

parseLine :: String -> (Int, [Int])
parseLine line
  | null ints = error "empty line"
  | otherwise = (head ints, tail ints)
  where
    ints = readInts line

calculateResult :: [(Int, [Int])] -> [Int]
calculateResult graphData
  | inGraph g 1 = dfs g 1
  | otherwise = error "there is no node 1 in given graph"
  where
    g = graph graphData

readInts :: String -> [Int]   
readInts = map readInt . words

readInt :: String -> Int
readInt l = case readMaybe l of
  Just x -> x
  Nothing -> error $ "not a number: " ++ l