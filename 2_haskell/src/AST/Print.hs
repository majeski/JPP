{-# OPTIONS_GHC -fno-warn-orphans #-}

module AST.Print 
( printAST
, Pretty (..)
) where

import AST.Types

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

printAST :: (Pretty a) => a -> String
printAST = prettyShow

nest' :: Doc -> Doc
nest' = nest 4 

instance Pretty Program where
    pPrint (Program code functions) = vcat $
        [ text "main:" <+> text "{"
        , nest' $ pPrint code
        , text "}"
        , text "" ] ++ map pPrint functions
        
instance Pretty Function where
    pPrint (Function name args retT code) = vcat
        [ text "func" <+> text name <> 
            hsep [parens $ commaSep args, text "->", pPrint retT, text "{"]
        , nest' $ pPrint code
        , text "}"
        , text "" ]

instance Pretty Stmt where
    pPrint (SList l) = vcat $ map pPrint l
    pPrint (SVar n t e) = text "var" <+> text n <> pPrint' t <+> text "=" <+> pPrint e <> semi
    pPrint (SLet n t e) = text "let" <+> text n <> pPrint' t <+> text "=" <+> pPrint e <> semi
    pPrint (SVarTuple tp t e) = 
        text "var" <+> pPrint tp <> pPrint' t <+> text "=" <+> pPrint e <> semi
    pPrint (SLetTuple tp t e) =
        text "let" <+> pPrint tp <> pPrint' t <+> text "=" <+> pPrint e <> semi
    pPrint (SFunction f) = pPrint f
    pPrint (SExpr e) = pPrint e <> semi
    pPrint (SIf e s Nothing) = vcat
        [ text "if" <+> pPrint e <+> text " {"
        , nest' $ pPrint s
        , text "}" ]
    pPrint (SIf e s1 (Just s2)) = vcat
        [ text "if" <+> pPrint e <+> text "{"
        , nest' $ pPrint s1
        , text "} else {"
        , nest' $ pPrint s2
        , text "}" ]
    pPrint (SWhile e s) = vcat
        [ text "while" <+> pPrint e <+> text "{"
        , nest' $ pPrint s
        , text "}" ]
    pPrint (SFor v r s) = vcat
        [ text "for" <+> text v <+> text "in" <+> pPrint r <+> text "{"
        , nest' $ pPrint s
        , text "}" ]
    pPrint (SReturn e) = text "return" <+> pPrint e <> semi
    pPrint (SAssign op v e) = text v <+> pPrint op <+> pPrint e <> semi
    pPrint (STupleAssign t e) = pPrint t <+> text "=" <+> pPrint e <> semi
    pPrint (SInc v) = text v <> text "++" <> semi
    pPrint (SDec v) = text v <> text "--" <> semi
    pPrint (SPrint l) = text "print" <+> commaSep l <> semi

pPrint' :: Maybe Type -> Doc
pPrint' (Just x) = colon <+> pPrint x
pPrint' Nothing = empty

instance Pretty Tuple where
    pPrint (Tuple terms) = parens $ commaSep terms

instance Pretty TupleTerm where
    pPrint (TupleTermTuple t) = pPrint t
    pPrint (TupleTermVar v) = text v

instance Pretty Range where
    pPrint (RExclusive a b) = pPrint a <> text "..<" <> pPrint b
    pPrint (RInclusive a b) = pPrint a <> text "..." <> pPrint b

instance Pretty AssignOp where
    pPrint SEq = text "="
    pPrint SAdd = text "+="
    pPrint SSub = text "-="
    pPrint SMul = text "*="
    pPrint SDiv = text "/="
    pPrint SMod = text "%="

instance Pretty TypedVar where
    pPrint (TypedVar n t) = text n <> colon <+> pPrint t

instance Pretty Expr where
    pPrint (EVar x) = text x
    pPrint (EInt x) = text $ show x 
    pPrint (EString x) = doubleQuotes $ text x
    pPrint (EBool x) = if x then text "true" else text "false"
    pPrint (ETuple xs) = parens $ commaSep xs
    pPrint (EBinOp op a b) = parens $ pPrint a <+> pPrint op <+> pPrint b
    pPrint (ECall f args) = pPrint f <> parens (commaSep args)
    pPrint (ELambda args retT code) = vcat
        [ hsep [text "lambda", parens $ commaSep args, text "->", pPrint retT, text "{"]
        , nest' $ pPrint code
        , text "}" ]

instance Pretty BinOp where
    pPrint EMul = text "*"
    pPrint EDiv = text "/"
    pPrint EMod = text "%"
    pPrint EAdd = text "+"
    pPrint ESub = text "-"
    pPrint ELess = text "<"
    pPrint ELessE = text "<="
    pPrint EGreater = text ">"
    pPrint EGreaterE = text ">="
    pPrint EEq = text "=="
    pPrint ENeq = text "!="
    pPrint EAnd = text "&&"
    pPrint EOr = text "||"
    pPrint EBind = text "::"

instance Pretty Type where
    pPrint TInt = text "Int"
    pPrint TBool = text "Bool"
    pPrint TString = text "String"
    pPrint (TTuple l) = parens $ commaSep l 
    pPrint (TFunc l) = parens $
        if length l == 1 then
            text "Void -> " <+> hcat (punctuate (text " -> ") $ map pPrint l)
        else
            hcat $ punctuate (text " -> ") (map pPrint l)

instance Pretty TypeSkeleton where
    pPrint TSTerm = text "?"
    pPrint (TSTuple l) = parens $ commaSep l

commaSep :: Pretty a => [a] -> Doc
commaSep args = hsep $ punctuate comma $ map pPrint args
