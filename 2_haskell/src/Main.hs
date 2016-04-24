import System.IO
import System.Environment
import Control.Monad

import AST.Parse
import AST.Print

main :: IO ()
main = do
    filename <- liftM head getArgs
    withFile filename ReadMode run

run :: Handle -> IO ()
run handle = do
    content <- hGetContents handle
    case parseAST content of
        Right x -> putStrLn (show x) >> putStrLn (printAST x)
        Left e -> putStrLn $ show e
