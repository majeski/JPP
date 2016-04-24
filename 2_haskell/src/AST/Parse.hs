{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module AST.Parse (parseAST) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import AST.Types

languageDef :: Token.LanguageDef st
languageDef = emptyDef {
    Token.identStart = letter <|> char '_',
    Token.identLetter = letter <|> char '_' <|> alphaNum,
    Token.opStart = Token.opLetter languageDef,
    Token.opLetter = oneOf "-+*/%<>=!|&:",
    Token.reservedOpNames = 
        [ "*", "/", "%"
        , "+", "-"
        , "<", ">", "<=", ">=", "==", "!="
        , "&&"
        , "||"
        , "::"
        , "++", "--", "...", "..<", "->" ],
    Token.reservedNames = 
        [ "main:", "func"
        , "true", "false", "lambda"
        , "String", "Int", "Bool", "Void"
        , "let", "var", "if", "else", "for", "in", "while", "return", "print" ],
    Token.caseSensitive = True
}

Token.TokenParser{..} = Token.makeTokenParser languageDef

parseAST :: String -> Either ParseError Program
parseAST raw = parse (whiteSpace >> program <* eof) "" raw

-- Program

program :: Parser Program
program = do 
    main <- reserved "main:" >> blockStmt
    functions <- optionMaybe $ many1 function
    case functions of
        Just fs -> return $ Program main fs
        Nothing -> return $ Program main []

-- Functions

function :: Parser Function
function = do
    name <- reserved "func" >> identifier
    args <- argumentsList
    retT <- reserved "->" >> functionArgType
    code <- blockStmt
    return $ Function name args retT code

-- Expressions

expr :: Parser Expr
expr = buildExpressionParser table term
term = choice
    [ natural >>= return . EInt
    , stringLiteral >>= return . EString
    , boolLiteral >>= return . EBool
    , callExpr
    , lambdaExpr ]
table = map map' [binOps1, binOps2, binOps3, binOps4, binOps5, binOps6]
    where
    map' = uncurry map
    binOps1 = (binOpL,
        [ ("*", EMul)
        , ("/", EDiv)
        , ("%", EMod) ])
    binOps2 = (binOpL,
        [ ("+", EAdd)
        , ("-", ESub) ])
    binOps3 = (binOpN,
        [ ("<", ELess)
        , ("<=", ELessE)
        , (">", EGreater)
        , (">=", EGreaterE)
        , ("==", EEq)
        , ("!=", ENeq) ])
    binOps4 = (binOpL,
        [ ("&&", EAnd) ])
    binOps5 = (binOpL,
        [ ("||", EOr) ])
    binOps6 = (binOpL,
        [ ("::", EBind) ])
    binOpL = binOp AssocLeft
    binOpR = binOp AssocRight
    binOpN = binOp AssocNone
    binOp assoc (name, ctor) = 
        Infix (reservedOp name >> return (EBinOp ctor)) assoc

boolLiteral :: Parser Bool
boolLiteral = choice
    [ reserved "true" >> return True
    , reserved "false" >> return False ]

callExpr :: Parser Expr
callExpr = do
    fun <- (identifier >>= return . EVar) <|> parens expr
    args <- optionMaybe (many1 exprList)
    return $ createCall fun args
    where
    exprList = parens $ commaSep expr
    createCall f args = case args of
        Just l  -> foldl ECall f l
        Nothing -> f

lambdaExpr :: Parser Expr
lambdaExpr = do
    args <- reserved "lambda" >> argumentsList
    retType <- reservedOp "->" >> functionArgType
    code <- blockStmt
    return $ ELambda args retType code

argumentsList :: Parser [TypedVar]
argumentsList = parens $ commaSep typedIdentifier

typedIdentifier :: Parser TypedVar
typedIdentifier = do
    name <- identifier <* symbol ":"
    t <- typeLiteral
    return $ TypedVar name t

-- Statements

blockStmt :: Parser Stmt
blockStmt = braces stmt

stmt :: Parser Stmt
stmt = liftM SList $ many1 singleStmt

singleStmt :: Parser Stmt
singleStmt =
    try letStmt <* semi <|> 
    try varStmt <* semi <|> 
    try printStmt <* semi <|> 
    try flowStmt <|>
    try postfixStmt <* semi <|>
    try assignStmt <* semi <|>
    liftM SExpr expr <* semi

letStmt :: Parser Stmt 
letStmt = do
    (name, t) <- reserved "let" >> varName
    valExpr <- reservedOp "=" >> expr
    return $ SLet name t valExpr

varStmt :: Parser Stmt 
varStmt = do
    (name, t) <- reserved "var" >> varName
    valExpr <- reservedOp "=" >> expr
    return $ SVar name t valExpr

varName :: Parser (String, Maybe Type)
varName = do
    name <- identifier
    t <- optionMaybe $ colon >> typeLiteral
    return (name, t)

printStmt :: Parser Stmt
printStmt = reserved "print" >> (liftM SPrint $ expr `sepBy1` comma)

flowStmt :: Parser Stmt
flowStmt = ifStmt <|> forStmt <|> whileStmt <|> (returnStmt <* semi)

ifStmt :: Parser Stmt
ifStmt = do
    cond <- reserved "if" >> expr
    trueBlock <- blockStmt
    falseBlockOpt <- optionMaybe (reserved "else" >> blockStmt)
    case falseBlockOpt of
        Just falseBlock -> return $ SIfElse cond trueBlock falseBlock
        Nothing -> return $ SIf cond trueBlock

forStmt :: Parser Stmt
forStmt = do
    var <- reserved "for" >> identifier
    r <- reserved "in" >> range
    block <- blockStmt
    return $ SFor var r block

range :: Parser Range
range = try inRange <|> exRange

inRange :: Parser Range
inRange = do
    begin <- expr <* reservedOp "..."
    end <- expr
    return $ RInclusive begin end

exRange :: Parser Range
exRange = do
    begin <- expr <* reservedOp "..<"
    end <- expr
    return $ RExclusive begin end

whileStmt :: Parser Stmt
whileStmt = do
    cond <- reserved "while" >> expr
    block <- blockStmt
    return $ SWhile cond block

returnStmt :: Parser Stmt
returnStmt = reserved "return" >> expr >>= return . SReturn

postfixStmt :: Parser Stmt
postfixStmt = try
    (identifier <* reservedOp "++" >>= return . SInc) <|>
    (identifier <* reservedOp "--" >>= return . SDec)

assignStmt :: Parser Stmt
assignStmt = identifier >>= \var -> choice $ map (\x -> x var)
    [ assignP "=" SEq
    , assignP "+=" SAdd
    , assignP "-=" SSub
    , assignP "*=" SMul
    , assignP "/=" SDiv
    , assignP "%=" SMod ]
    where
    assignP op ctor var = 
        reservedOp op >> expr >>= return . SAssign ctor var

-- Types

typeLiteral :: Parser Type
typeLiteral = try functionType <|> simpleType

functionArgType :: Parser Type
functionArgType = simpleType <|> parens functionType

simpleType :: Parser Type
simpleType = choice 
    [ reserved "Int" >> return TInt
    , reserved "Bool" >> return TBool
    , reserved "String" >> return TString ]

functionType :: Parser Type
functionType = do
    args <- functionTypeNoArgs <|> functionTypeArgs
    return $ TFuncType args

functionTypeArgs :: Parser [Type]
functionTypeArgs = functionArgType `sepBy2` (reservedOp "->")

functionTypeNoArgs :: Parser [Type]
functionTypeNoArgs = do
    retT <- reserved "Void" >> reservedOp "->" >> functionArgType
    return [retT]

-- Helpers

sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 arg sep = do
    first <- arg <* sep
    rest <- arg `sepBy1` sep
    return $ first:rest


