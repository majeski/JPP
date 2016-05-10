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

import AST.Types

import Eval.Types
import qualified Eval.Builtins as Builtins

data RuntimeError
    = RENoReturn
    | REDivByZero
    | REBuiltinFailed String

instance Show RuntimeError where
    show RENoReturn = "function exited without return"
    show REDivByZero = "division by zero"
    show (REBuiltinFailed f) = "builtin function `" ++ f ++ "` failed"

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

registerBuiltin :: Builtin -> IM ()
registerBuiltin b = setVal (builtinName b) $ VFunction [] (FIBuiltin b)

-- Statements

evalStmt :: Stmt -> IM ()

evalStmt (SList l) = mapM_ evalStmt l

evalStmt (SVar n _ e) = evalExpr e >>= setVal n
evalStmt (SLet n _ e) = evalExpr e >>= setVal n

evalStmt (SLetTuple tup _ e) = evalExpr e >>= setTuple tup
evalStmt (SVarTuple tup _ e) = evalExpr e >>= setTuple tup

evalStmt (SFunction f) = do
    registerFunctionName f
    fixFunctionImpl f

evalStmt (SExpr e) = void $ evalExpr e

evalStmt (SIf condE trueS falseS) = localEval $
    evalExpr condE >>= \case
        VBool True -> evalStmt trueS
        _ -> mapM_ evalStmt falseS

evalStmt (SWhile condE stmt) = localEval loop
    where 
    loop = evalExpr condE >>= \case
        VBool True -> evalStmt stmt >> loop
        _ -> return ()

evalStmt (SFor n range stmt) = localEval $ do
    (begin, isEnd, next) <- evalRange range
    setVal n (VInt begin) >> loop isEnd next
    where
    loop isEnd next = getVal n >>= \(VInt i) ->
        unless (isEnd i) (evalStmt stmt >> updateIntVal n next >> loop isEnd next)

evalStmt (SReturn e) = do
    Env _ (Just c) <- get
    evalExpr e >>= c

evalStmt (SAssign SEq n e) = evalExpr e >>= updateVal n

evalStmt (SAssign op n e) = do
    VInt v <- evalExpr e
    case op of
        SAdd -> updateIntVal n (+v)
        SSub -> updateIntVal n $ flip (-) v
        SMul -> updateIntVal n (*v)
        SDiv ->
            when (v == 0) (throwError REDivByZero) >>
            updateIntVal n (`div` v)
        SMod -> 
            when (v == 0) (throwError REDivByZero) >>
            updateIntVal n (`mod` v)
        _ -> error "invalid SAssign operator"

evalStmt (STupleAssign tup e) = evalExpr e >>= updateTupleVal tup

evalStmt (SInc n) = updateIntVal n succ
evalStmt (SDec n) = updateIntVal n pred

evalStmt (SPrint l) = do
    vs <- mapM evalExpr l
    liftIO $ putStrLn $ unwords $ map (\(VString s) -> s) vs

-- Range

evalRange :: Range -> IM (Integer, Integer -> Bool, Integer -> Integer)
evalRange (RExclusive be ee) = do
    VInt b <- evalExpr be
    VInt e <- evalExpr ee
    return (b, (>=e), succ)
evalRange (RInclusive be ee) = do
    VInt b <- evalExpr be
    VInt e <- evalExpr ee
    let next = if b <= e then succ else pred
    return (b, (== next e), next)

-- Expressions

evalExpr :: Expr -> IM Value
evalExpr (EVar n) = getVal n
evalExpr (EBool x) = return $ VBool x
evalExpr (EString x) = return $ VString x
evalExpr (EInt x) = return $ VInt x
evalExpr (ETuple es) = VTuple <$> mapM evalExpr es
evalExpr (EBinOp op e1 e2) = evalBinOp op e1 e2
evalExpr (ELambda args _ stmt) = do
    Env mem _ <- get
    return $ VFunction [] $ FIUser (map argName args) stmt mem

evalExpr (ECall e callArgs) = do
    VFunction boundArgs impl <- evalExpr e
    argVals <- (reverse boundArgs ++) <$> mapM evalExpr callArgs
    case impl of
        FIBuiltin b -> case builtinFunc b argVals of
                Just res -> return res
                Nothing -> throwError $ REBuiltinFailed $ builtinName b
        FIUser argNames stmt mem -> localEval $ callCC $ \c -> do
            put $ Env mem (Just c)
            zipWithM_ setVal argNames argVals
            evalStmt stmt
            throwError RENoReturn

-- Binary operators

evalBinOp :: BinOp -> Expr -> Expr -> IM Value
evalBinOp EDiv e1 e2 = safeDivOp div e1 e2
evalBinOp EMod e1 e2 = safeDivOp mod e1 e2
evalBinOp EBind e1 e2 = do
    VFunction boundVals impl <- evalExpr e1
    v <- evalExpr e2
    return $ VFunction (v:boundVals) impl

evalBinOp op e1 e2 = liftM2 (opFunc op) (evalExpr e1) (evalExpr e2)

safeDivOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> IM Value
safeDivOp op e1 e2 = do
    VInt v1 <- evalExpr e1
    VInt v2 <- evalExpr e2
    when (v2 == 0) (throwError REDivByZero)
    return $ VInt $ v1 `op` v2

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
opFunc _ = error "invalid opFunc usage"

binOpII :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
binOpII op (VInt a) (VInt b) = VInt $ a `op` b
binOpII _ _ _ = error "invalid binOpII usage"

binOpIB :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
binOpIB op (VInt a) (VInt b) = VBool $ a `op` b
binOpIB _ _ _ = error "invalid binOpIB usage"

binOpBB :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
binOpBB op (VBool a) (VBool b) = VBool $ a `op` b
binOpBB _ _ _ = error "invalid binOpBB usage"

-- Helpers

registerFunctionName :: Function -> IM ()
registerFunctionName (Function name _ _ _) = setVal name VNone

fixFunctionImpl :: Function -> IM ()
fixFunctionImpl (Function name args _ stmt) = do
    Env mem _ <- get
    updateVal name $ VFunction [] $ FIUser (map argName args) stmt mem

localEval :: IM a -> IM a
localEval m = get >>= \st -> m <* put st

getVal :: String -> IM Value
getVal name = do
    Env mem _ <- get
    liftIO $ readIORef $ mem ! name

setTuple :: Tuple -> Value -> IM ()
setTuple (Tuple terms) (VTuple vs) = zipWithM_ setTupleTerm terms vs
setTuple _ _ = error "invalid setTuple usage"

setTupleTerm :: TupleTerm -> Value -> IM ()
setTupleTerm (TupleTermTuple tup) = setTuple tup
setTupleTerm (TupleTermVar v) = setVal v

setVal :: String -> Value -> IM ()
setVal n v = do
    vRef <- liftIO $ newIORef v
    modify $ \(Env mem c) -> Env (Map.insert n vRef mem) c

updateTupleVal :: Tuple -> Value -> IM ()
updateTupleVal (Tuple terms) (VTuple vs) = zipWithM_ updateTupleTermVal terms vs
updateTupleVal _ _ = error "invalid updateTupleVal usage"

updateTupleTermVal :: TupleTerm -> Value -> IM ()
updateTupleTermVal (TupleTermTuple tup) = updateTupleVal tup
updateTupleTermVal (TupleTermVar v) = updateVal v

updateVal :: String -> Value -> IM ()
updateVal n v = do
    Env mem _ <- get
    liftIO $ modifyIORef' (mem ! n) $ const v

updateIntVal :: String -> (Integer -> Integer) -> IM ()
updateIntVal n f = do
    Env mem _ <- get
    liftIO $ modifyIORef' (mem ! n) $ \(VInt x) -> VInt $ f x
