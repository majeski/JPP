{-# LANGUAGE LambdaCase #-}

import Control.Monad
import System.IO
import System.Environment

import AST.Types
import AST.Parse
import Eval.TypeCheck
import Eval.Runtime

main :: IO ()
main = do
    filename <- head <$> getArgs
    withFile filename ReadMode interpreter

interpreter :: Handle -> IO ()
interpreter handle = do
    content <- hGetContents handle
    let result = parse content >>= checkTypes
    case result of
        Left err -> putStrLn $ "ERR:\n" ++ err
        Right p -> run p

parse :: String -> Either String Program
parse raw = case parseAST raw of
    Left err -> Left $ show err
    Right p -> Right p

checkTypes :: Program -> Either String Program
checkTypes p = case typeCheck p of
    Left err -> Left $ show err
    Right _ -> Right p

run :: Program -> IO ()
run p = void $ evalProgram p >>= \case
    Left err -> putStrLn $ "Runtime error: " ++ show err
    Right _ -> return ()
