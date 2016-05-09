{-# LANGUAGE LambdaCase #-}

module Eval.TypeCheck
( TypeCheckError
, typeCheck )
where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.List

import AST.Types
import AST.Print

import Eval.Types
import qualified Eval.Builtins as Builtins

typeCheck :: Program -> Either TypeCheckError ()
typeCheck p = runExcept $ evalStateT (programType p) Map.empty

type TCM = StateT VarSet (Except TypeCheckError)
type VarSet = Map String VarInfo

type VarInfo = ((Access, Scope), Type)
data Scope = OuterScope | CurrentScope
data Access = Readonly | ReadWrite

data TypeCheckError
    = TCReturnOutside
    | TCInvalidBind
    | TCNotAFunction Type
    | TCUndefinedVar String
    | TCRedeclaration String
    | TCReadonlyVar String
    | TCInvalidType Type Type
    | TCInvalidCall [Type] [Type]
    | TCInvalidMatch TypeSkeleton Type
    | TypeCheckErrorIn Stmt TypeCheckError
    | TypeCheckErrorInE Expr TypeCheckError

instance Show TypeCheckError where
    show TCReturnOutside = "return outside of a function"
    show TCInvalidBind = "cannot bind value to function without arguments"
    show (TCNotAFunction actual) = "not a function type: " ++ showType actual
    show (TCUndefinedVar name) = "undefined identifier '" ++ name ++ "'"
    show (TCRedeclaration name) = "cannot redeclare identifier'" ++ name ++ "' in the same scope"
    show (TCReadonlyVar name) = "invalid operation on readonly identifier '" ++ name ++ "'"
    show (TCInvalidType expected actual) = intercalate "\n"
        [ "invalid type"
        , "expected: " ++ showType expected
        , "  actual: " ++ showType actual ]
    show (TCInvalidCall fargs args) = intercalate "\n"
        [ "invalid function call"
        , "expected arguments: " ++ intercalate ", " (map showType fargs)
        , "  actual arguments: " ++ intercalate ", " (map showType args) ]
    show (TCInvalidMatch ts t) = intercalate "\n"
        [ "invalid match"
        , "   expected: " ++ printAST ts
        , "     actual: " ++ showType t ]
    show (TypeCheckErrorIn stmt err) = concat 
        [ show err
        , "\nin:\n"
        , printAST stmt ]
    show (TypeCheckErrorInE expr err) = concat
        [ show err
        , "\nin:\n"
        , printAST expr ]

showType :: Type -> String
showType (TFunc [x]) = "Void -> " ++ showType' x
showType (TFunc l) = intercalate " -> " $ map showType' l
showType t = printAST t

showType' :: Type -> String
showType' x@(TFunc _) = "(" ++ showType x ++ ")"
showType' x = showType x

-- Program

programType :: Program -> TCM ()
programType (Program main fs) = do
    mapM_ addBuiltinFunc Builtins.builtins
    createScope
    mapM_ addFunc fs
    mapM_ (\(Function _ args retT s) -> functionImplType args retT s) fs
    localScope $ stmtType main

-- Statements

stmtType :: Stmt -> TCM ()
stmtType (SList l) = forM_ l stmtType
stmtType s = catchError (stmtType' s) (throwError . TypeCheckErrorIn s)

stmtType' :: Stmt -> TCM ()
stmtType' (SList _) = error "invalid stmtType' usage"

stmtType' (SVar name optT e) = declareVar name optT e ReadWrite
stmtType' (SLet name optT e) = declareVar name optT e Readonly
stmtType' (SVarTuple tup optT e) = declareTuple tup optT e ReadWrite
stmtType' (SLetTuple tup optT e) = declareTuple tup optT e Readonly

stmtType' (SFunction f@(Function _ args retT stmt)) = do
    addFunc f
    functionImplType args retT stmt

stmtType' (SExpr e) = void $ exprType e

stmtType' (SIf e trueS falseS) = do
    exprType e >>= expectType TBool
    localScope $ stmtType trueS
    localScope $ mapM_ stmtType falseS

stmtType' (SWhile e stmt) = localScope $ do
    exprType e >>= expectType TBool
    stmtType stmt

stmtType' (SFor n r stmt) = localScope $ do
    rangeType r
    setVar n TInt Readonly
    stmtType stmt

stmtType' (SReturn e) = do
    eT <- exprType e
    returnType >>= \case
        Just rT -> expectType rT eT
        Nothing -> throwError TCReturnOutside

stmtType' (SAssign SEq n e) = do
    expectRWVar n
    vT <- varType n
    exprType e >>= expectType vT

stmtType' (SAssign _ n e) = do
    expectRWVar n
    varType n >>= expectType TInt
    exprType e >>= expectType TInt

stmtType' (STupleAssign t e) = do
    expectRWTuple t
    tupT <- tupleType t
    exprType e >>= expectType tupT

stmtType' (SInc n) = do
    expectRWVar n
    varType n >>= expectType TInt

stmtType' (SDec n) = do
    expectRWVar n
    varType n >>= expectType TInt

stmtType' (SPrint es) = mapM_ (exprType >=> expectType TString) es 

declareVar :: String -> Maybe Type -> Expr -> Access -> TCM ()
declareVar name optT e access = do
    eT <- exprType e
    expectOptType optT eT
    setVar name eT access

declareTuple :: Tuple -> Maybe Type -> Expr -> Access -> TCM ()
declareTuple tup optT e access = do
    eT <- exprType e
    expectOptType optT eT
    tSkeleton <- tupleTypeSkeleton tup
    expectMatch tSkeleton eT
    setTuple tup eT access

-- Range

rangeType :: Range -> TCM ()
rangeType (RExclusive e1 e2) = do
    exprType e1 >>= expectType TInt
    exprType e2 >>= expectType TInt
rangeType (RInclusive e1 e2) = do 
    exprType e1 >>= expectType TInt
    exprType e2 >>= expectType TInt

-- Expressions

exprType :: Expr -> TCM Type
exprType e = catchError (exprType' e) (throwError . TypeCheckErrorInE e)

exprType' :: Expr -> TCM Type

exprType' (EBool _) = return TBool
exprType' (EInt _) = return TInt
exprType' (EString _) = return TString

exprType' (EVar name) = expectVar name >> varType name

exprType' (ETuple es) = TTuple <$> mapM exprType es

exprType' (EBinOp EMul e1 e2) = binOpType e1 e2 TInt 
exprType' (EBinOp EDiv e1 e2) = binOpType e1 e2 TInt
exprType' (EBinOp EMod e1 e2) = binOpType e1 e2 TInt 
exprType' (EBinOp EAdd e1 e2) = binOpType e1 e2 TInt 
exprType' (EBinOp ESub e1 e2) = binOpType e1 e2 TInt 
exprType' (EBinOp ELess e1 e2) = binOpType e1 e2 TInt >> return TBool
exprType' (EBinOp ELessE e1 e2) = binOpType e1 e2 TInt >> return TBool
exprType' (EBinOp EGreater e1 e2) = binOpType e1 e2 TInt >> return TBool
exprType' (EBinOp EGreaterE e1 e2) = binOpType e1 e2 TInt >> return TBool
exprType' (EBinOp EEq e1 e2) = binOpType e1 e2 TInt >> return TBool
exprType' (EBinOp ENeq e1 e2) = binOpType e1 e2 TInt >> return TBool
exprType' (EBinOp EAnd e1 e2) = binOpType e1 e2 TBool
exprType' (EBinOp EOr e1 e2) = binOpType e1 e2 TBool

exprType' (EBinOp EBind f arg) = do
    ft <- exprType f
    argt <- exprType arg
    case ft of
        TFunc (farg:args) -> do 
            expectType farg argt
            return $ TFunc args
        TFunc [] -> throwError TCInvalidBind
        _ -> throwError $ TCNotAFunction ft 

exprType' (ECall f args) = do 
    ft <- exprType f
    argst <- mapM exprType args
    case ft of
        TFunc fargst ->
            if init fargst /= argst then
                throwError $ TCInvalidCall (init fargst) argst
            else 
                return $ last fargst
        _ -> throwError $ TCNotAFunction ft

exprType' (ELambda args retT stmt) = do
    functionImplType args retT stmt
    return $ TFunc $ map argType args ++ [retT]

-- Function impl

functionImplType :: [TypedVar] -> Type -> Stmt -> TCM ()
functionImplType args retT stmt = localScope $ do
    allReadonly
    mapM_ (\(TypedVar n t) -> setVar n t Readonly) args
    setReturnType retT
    stmtType stmt

-- Tuple

tupleType :: Tuple -> TCM Type
tupleType (Tuple terms) = TTuple <$> mapM tupleTermType terms

tupleTermType :: TupleTerm -> TCM Type
tupleTermType (TupleTermTuple tp) = tupleType tp
tupleTermType (TupleTermVar v) = expectVar v >> varType v
    
tupleTypeSkeleton :: Tuple -> TCM TypeSkeleton
tupleTypeSkeleton (Tuple terms) = TSTuple <$> mapM tupleTermTypeSkeleton terms

tupleTermTypeSkeleton :: TupleTerm -> TCM TypeSkeleton
tupleTermTypeSkeleton (TupleTermTuple tp) = tupleTypeSkeleton tp
tupleTermTypeSkeleton (TupleTermVar _) = return TSTerm

-- Helpers

-- lhs -> rhs -> expected type for lhs & rhs
binOpType :: Expr -> Expr -> Type -> TCM Type
binOpType e1 e2 t = do
    exprType e1 >>= expectType t
    exprType e2 >>= expectType t
    return t

addFunc :: Function -> TCM ()
addFunc (Function name args retT _) = setVar name t Readonly
    where
    t = TFunc $ map argType args ++ [retT]

addBuiltinFunc :: Builtin -> TCM ()
addBuiltinFunc f = setVar n t Readonly
    where
    n = builtinName f
    t = builtinType f 

setReturnType :: Type -> TCM ()
setReturnType t = setVar returnTypeID t Readonly

setTuple :: Tuple -> Type -> Access -> TCM ()
setTuple (Tuple terms) (TTuple ts) access = 
    zipWithM_ (\term t -> setTupleTerm term t access) terms ts
setTuple _ _ _ = error "incorrect setTuple usage"

setTupleTerm :: TupleTerm -> Type -> Access -> TCM  ()
setTupleTerm (TupleTermVar v) = setVar v
setTupleTerm (TupleTermTuple tup) = setTuple tup

setVar :: String -> Type -> Access -> TCM ()
setVar n t access = do
    expectFreeIdentifier n
    modify $ Map.insert n ((access, CurrentScope), t)

localScope :: TCM a -> TCM a
localScope m = do
    st <- get
    createScope >> m <* put st

allReadonly :: TCM ()
allReadonly = modify $ Map.map (\((_, scope), n) -> ((Readonly, scope), n)) 

createScope :: TCM ()
createScope =  modify $ Map.map (\((access, _), n) -> ((access, OuterScope), n))

expectMatch :: TypeSkeleton -> Type -> TCM ()
expectMatch ts t = expectMatch' ts t ts t

expectMatch' :: TypeSkeleton -> Type -> TypeSkeleton -> Type -> TCM ()
expectMatch' outerTs outerT (TSTuple terms) (TTuple t) = do
    when (length terms /= length t) (throwError $ TCInvalidMatch outerTs outerT)
    zipWithM_ (expectMatch' outerTs outerT) terms t
expectMatch' _ _ TSTerm _ = return ()
expectMatch' _ _ _ _ = error "incorrect expectMatch' usage"

expectOptType ::Maybe Type -> Type -> TCM ()
expectOptType Nothing _ = return ()
expectOptType (Just expected) t = expectType expected t

expectType :: Type -> Type -> TCM ()
expectType expected actual =
    when (expected /= actual) (throwError $ TCInvalidType expected actual)

expectVar :: String -> TCM ()
expectVar name =
    varExist name >>= flip unless (throwError $ TCUndefinedVar name)

expectRWVar :: String -> TCM ()
expectRWVar name = do 
    expectVar name
    varReadonly name >>= flip when (throwError $ TCReadonlyVar name)

expectRWTuple :: Tuple -> TCM ()
expectRWTuple (Tuple terms) = mapM_ expectRWTupleTerm terms
    
expectRWTupleTerm :: TupleTerm -> TCM ()
expectRWTupleTerm (TupleTermTuple t) = expectRWTuple t
expectRWTupleTerm (TupleTermVar v) = expectRWVar v

expectFreeIdentifier :: String -> TCM ()
expectFreeIdentifier name = 
    varFromCurrentScope name >>= flip when (throwError $ TCRedeclaration name)

returnType :: TCM (Maybe Type)
returnType = do
    st <- get
    return $ snd <$> Map.lookup returnTypeID st

returnTypeID :: String
returnTypeID = "##return_type"

varFromCurrentScope :: String -> TCM Bool
varFromCurrentScope v = do
    st <- get
    varExist v >>= \case
        False -> return False
        True -> return $ case snd . fst $ st ! v of
            CurrentScope -> True
            OuterScope -> False

varExist :: String -> TCM Bool
varExist v = gets $ Map.member v

varReadonly :: String -> TCM Bool
varReadonly v = do
    st <- get
    case fst . fst $ st ! v of
        Readonly -> return True
        _ -> return False

varType :: String -> TCM Type
varType v = gets $ snd . (!v)
