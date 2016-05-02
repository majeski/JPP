module Eval.Types
( Value (..)
, Builtin (..)
, FunctionImpl (..)
, Memory
) where

import Data.Map
import Data.IORef

import AST.Types

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VFunction [Value] FunctionImpl
    | VNone

data FunctionImpl
    = FIBuiltin Builtin
    | FIUser [String] Stmt Memory

data Builtin = Builtin {
    name :: String,
    fType :: Type,
    assocF :: [Value] -> Value
}

type Memory = Map String (IORef Value)
