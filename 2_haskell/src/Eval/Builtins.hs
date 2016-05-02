module Eval.Builtins
( toString
, fromString
, builtins
) where

import AST.Types
import Eval.Types

toString :: Builtin
toString = Builtin {
    name = "toString",
    fType = TFunc [TInt, TString],
    assocF = \[(VInt arg)] -> VString $ show arg
}

fromString :: Builtin
fromString = Builtin {
    name = "fromString",
    fType = TFunc [TString, TInt],
    assocF = \[(VString arg)] -> VInt $ read arg
}

builtins :: [Builtin]
builtins = [toString, fromString]
