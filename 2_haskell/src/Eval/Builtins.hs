module Eval.Builtins
( toString
, fromString
, builtins
) where

import Text.Read

import AST.Types
import Eval.Types

toString :: Builtin
toString = Builtin {
    builtinName = "toString",
    builtinType = TFunc [TInt, TString],
    builtinFunc = \[VInt arg] -> Just $ VString $ show arg
}

fromString :: Builtin
fromString = Builtin {
    builtinName = "fromString",
    builtinType = TFunc [TString, TInt],
    builtinFunc = \[VString arg] -> VInt <$> readMaybe arg
}

builtins :: [Builtin]
builtins = [toString, fromString]
