module Eval.Types
( Value (..)
, Builtin (..)
, FunctionImpl (..)
, Memory
) where

import Data.Map (Map)
import Data.IORef

import AST.Types

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VTuple [Value]
    | VFunction [Value] FunctionImpl
    | VNone

data FunctionImpl
    = FIBuiltin Builtin
    | FIUser [String] Stmt Memory

data Builtin = Builtin {
    builtinName :: String,
    builtinType :: Type,
    builtinFunc :: [Value] -> Value
}

type Memory = Map String (IORef Value)
