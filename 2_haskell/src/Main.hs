import System.IO
import System.Environment
import Control.Monad

import AST.Parse
import AST.Print
import AST.TypeCheck

main :: IO ()
main = do
    filename <- liftM head getArgs
    withFile filename ReadMode run

run :: Handle -> IO ()
run handle = do
    content <- hGetContents handle
    case parseAST content of
        Right x -> do
            putStrLn $ show x
            putStrLn $ printAST x
            case typeCheck x of
                Right _ -> putStrLn "types OK"
                Left err -> putStrLn $ show err
        Left e -> putStrLn $ show e
