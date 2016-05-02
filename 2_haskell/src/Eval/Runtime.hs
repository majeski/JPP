{-# LANGUAGE LambdaCase #-}

module Eval.Runtime
( Value (..)
, RuntimeError (..)
, evalProgram
) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.Trans.Cont (evalContT)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe
import Data.List

import AST.Types

import Eval.Types (Value (..), FunctionImpl (..), Memory)
import qualified Eval.Types as EvalTypes
import qualified Eval.Builtins as Builtins

data RuntimeError
    = RENoReturn
    | REDivByZero

instance Show (RuntimeError) where
    show RENoReturn = "function exited without return"
    show REDivByZero = "division by zero"

type IM = StateT Env (ExceptT RuntimeError (ContT (Either RuntimeError ()) IO))
data Env = Env Memory (Maybe (Value -> IM ()))

-- Program

evalProgram :: Program -> IO (Either RuntimeError ())
evalProgram (Program stmt functions) =
    evalContT $ runExceptT $ evalStateT initialState initialEnv
    where
    initialState = do
        mapM_ registerBuiltin Builtins.builtins
        mapM_ registerFunctionName functions
        mapM_ fixFunctionImpl functions
        evalStmt stmt

initialEnv :: Env
initialEnv = Env Map.empty Nothing

registerBuiltin :: EvalTypes.Builtin -> IM ()
registerBuiltin b = setVal (EvalTypes.name b) $ VFunction [] (FIBuiltin b)

registerFunctionName :: Function -> IM ()
registerFunctionName (Function name _ _ _) = setVal name $ VNone

fixFunctionImpl :: Function -> IM ()
fixFunctionImpl (Function name args _ stmt) = do
    Env mem _ <- get
    updateVal name $ VFunction [] $ FIUser (map argName args) stmt mem              

-- Statements

evalStmt :: Stmt -> IM ()

evalStmt (SList l) = mapM_ evalStmt l

evalStmt (SVar n _ e) = evalExpr e >>= setVal n
evalStmt (SLet n _ e) = evalExpr e >>= setVal n

evalStmt (SExpr e) = void $ evalExpr e

evalStmt (SIf condE trueS falseS) = localEval $
    evalExpr condE >>= \case
        VBool True -> evalStmt trueS
        _ -> when (isJust falseS) (evalStmt $ fromJust falseS) 

evalStmt (SWhile condE stmt) = localEval loop
    where 
    loop = evalExpr condE >>= \case
        VBool True -> evalStmt stmt >> loop
        _ -> return ()

evalStmt (SFor n range stmt) = localEval $ do
    (begin, end, next) <- evalRange range
    setVal n (VInt begin) >> loop end next
    where
    loop end next = getVal n >>= \(VInt i) ->
        if i == end then
            return ()
        else
            evalStmt stmt >> updateIntVal n next >> loop end next

evalStmt (SReturn e) = do
    Env _ (Just c) <- get
    evalExpr e >>= c

evalStmt (SAssign SEq n e) = evalExpr e >>= updateVal n 

evalStmt (SAssign op n e) = do
    VInt v <- evalExpr e
    case op of
        SAdd -> updateIntVal n $ flip (+) v
        SSub -> updateIntVal n $ flip (-) v
        SMul -> updateIntVal n $ flip (*) v
        SDiv ->
            when (v == 0) (throwError REDivByZero) >>
            updateIntVal n (flip div v)
        SMod -> 
            when (v == 0) (throwError REDivByZero) >>
            updateIntVal n (flip mod v)
        _ -> undefined

evalStmt (SInc n) = updateIntVal n succ
evalStmt (SDec n) = updateIntVal n pred

evalStmt (SPrint l) = do
    vs <- mapM evalExpr l
    liftIO $ putStrLn $ intercalate " " $ map (\(VString s) -> s) vs

-- Range

evalRange :: Range -> IM (Integer, Integer, Integer -> Integer)
evalRange (RExclusive be ee) = do
    VInt b <- evalExpr be
    VInt e <- evalExpr ee
    return (b, e, succ)
evalRange (RInclusive be ee) = do
    VInt b <- evalExpr be
    VInt e <- evalExpr ee
    let next = if b <= e then succ else pred
    return (b, next e, next)

-- Expressions

evalExpr :: Expr -> IM Value
evalExpr (EVar n) = getVal n
evalExpr (EBool x) = return $ VBool x
evalExpr (EString x) = return $ VString x
evalExpr (EInt x) = return $ VInt x
evalExpr (EBinOp op e1 e2) = evalBinOp op e1 e2
evalExpr (ELambda args _ stmt) = do
    Env mem _ <- get
    return $ VFunction [] $ FIUser (map argName args) stmt mem

evalExpr (ECall e callArgs) = do
    VFunction boundArgs impl <- evalExpr e
    argVals <- (++) (reverse boundArgs) <$> mapM evalExpr callArgs
    case impl of
        FIBuiltin b -> return $ EvalTypes.assocF b argVals
        FIUser argNames stmt mem -> localEval $ callCC $ \c -> do
            put $ Env mem (Just c)
            mapM_ (uncurry setVal) (zip argNames argVals)
            evalStmt stmt
            throwError RENoReturn

-- Binary operators

evalBinOp :: BinOp -> Expr -> Expr -> IM Value
evalBinOp EDiv e1 e2 = do
    VInt v1 <- evalExpr e1 
    VInt v2 <- evalExpr e2
    when (v2 == 0) (throwError REDivByZero)
    return $ VInt $ v1 `div` v2
evalBinOp EMod e1 e2 = do
    VInt v1 <- evalExpr e1 
    VInt v2 <- evalExpr e2
    when (v2 == 0) (throwError REDivByZero)
    return $ VInt $ v1 `mod` v2
evalBinOp EBind e1 e2 = do
    VFunction boundVals impl <- evalExpr e1
    v <- evalExpr e2
    return $ VFunction (v:boundVals) impl

evalBinOp op e1 e2 = liftM2 (opFunc op) (evalExpr e1) (evalExpr e2)

opFunc :: BinOp -> (Value -> Value -> Value)
opFunc EMul = binOpII (*)
opFunc EAdd = binOpII (+)
opFunc ESub = binOpII (-)
opFunc ELess = binOpIB (<)
opFunc ELessE = binOpIB (<=)
opFunc EGreater = binOpIB (>)
opFunc EGreaterE = binOpIB (>=)
opFunc EEq = binOpIB (==)
opFunc ENeq = binOpIB (/=)
opFunc EAnd = binOpBB (&&)
opFunc EOr = binOpBB (||)
opFunc _ = undefined

binOpII :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
binOpII op (VInt a) (VInt b) = VInt $ a `op` b
binOpII _ _ _ = undefined

binOpIB :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
binOpIB op (VInt a) (VInt b) = VBool $ a `op` b
binOpIB _ _ _ = undefined

binOpBB :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
binOpBB op (VBool a) (VBool b) = VBool $ a `op` b
binOpBB _ _ _ = undefined

-- Helpers

argName :: TypedVar -> String
argName (TypedVar n _) = n

localEval :: IM a -> IM a
localEval m = get >>= \st -> m <* put st

getVal :: String -> IM Value
getVal name = do
    Env mem _ <- get
    v <- liftIO $ readIORef $ mem ! name
    return v

setVal :: String -> Value -> IM ()
setVal n v = do
    vRef <- liftIO $ newIORef v
    modify $ \(Env mem c) -> Env ((Map.alter (\_ -> Just vRef) n) mem) c

updateVal :: String -> Value -> IM ()
updateVal n v = do
    Env mem _ <- get
    liftIO $ modifyIORef' (mem ! n) $ \_ -> v

updateIntVal :: String -> (Integer -> Integer) -> IM ()
updateIntVal n f = do
    Env mem _ <- get
    liftIO $ modifyIORef' (mem ! n) $ \(VInt x) -> VInt $ f x
