module AST.Types
( Program (..)
, Function (..)
, Expr (..)
, BinOp (..)
, Stmt (..)
, Range (..)
, TypedVar (..)
, Type (..)
, AssignOp (..)
) where

data Program
    = Program Stmt [Function]
    deriving (Eq, Show)

data Function
    = Function String [TypedVar] Type Stmt
    deriving (Eq, Show)

data Expr
    = EVar String
    | EBool Bool
    | EString String
    | EInt Integer
    | EBinOp BinOp Expr Expr
    | ECall Expr [Expr]
    | ELambda [TypedVar] Type Stmt
    deriving (Eq, Show)

data BinOp
    = EMul | EDiv | EMod
    | EAdd | ESub
    | ELess | ELessE | EGreater | EGreaterE | EEq | ENeq
    | EAnd
    | EOr
    | EBind 
    deriving (Eq, Show)

data Stmt
    = SList [Stmt]
    | SVar String (Maybe Type) Expr
    | SLet String (Maybe Type) Expr
    | SExpr Expr
    | SIf Expr Stmt (Maybe Stmt)
    | SWhile Expr Stmt
    | SFor String Range Stmt
    | SReturn Expr
    | SAssign AssignOp String Expr
    | SInc String
    | SDec String
    | SPrint [Expr]
    deriving (Eq, Show)

data Range
    = RExclusive Expr Expr
    | RInclusive Expr Expr 
    deriving (Eq, Show)

data TypedVar 
    = TypedVar String Type
    deriving (Eq, Show)

data Type
    = TInt
    | TBool
    | TString
    | TFunc [Type] 
    deriving (Eq, Show)

data AssignOp
    = SEq
    | SAdd
    | SSub
    | SMul
    | SDiv
    | SMod 
    deriving (Eq, Show)
