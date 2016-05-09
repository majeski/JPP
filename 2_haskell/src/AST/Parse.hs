{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module AST.Parse 
( parseAST
, parseExpr
) where

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
parseAST = parse (whiteSpace >> program <* eof) "" 

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace >> expr <* eof) "" 

-- Program

program :: Parser Program
program = do 
    main <- reserved "main:" >> blockStmt
    optionMaybe (many1 function) >>= \case
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
    [ EInt <$> natural
    , EString <$> stringLiteral 
    , EBool <$> boolLiteral
    , try tupleExpr
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

tupleExpr :: Parser Expr
tupleExpr = ETuple <$> parens (expr `sepBy2` comma)

callExpr :: Parser Expr
callExpr = do
    fun <- (EVar <$> identifier) <|> parens expr
    args <- optionMaybe $ many1 exprList
    return $ createCall fun args
    where
    exprList = parens $ commaSep expr
    createCall f args = case args of
        Just l -> foldl ECall f l
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
stmt = SList <$> many singleStmt

singleStmt :: Parser Stmt
singleStmt = choice
    [ try letStmt <* semi 
    , try varStmt <* semi 
    , try letTupleStmt <* semi
    , try varTupleStmt <* semi
    , try funcStmt
    , try printStmt <* semi 
    , try flowStmt
    , try postfixStmt <* semi
    , try assignStmt <* semi
    , try tupleAssignStmt <* semi
    , SExpr <$> expr <* semi ]

letStmt :: Parser Stmt 
letStmt = do
    name <- reserved "let" >> identifier
    t <- varSygnature
    valExpr <- reservedOp "=" >> expr
    return $ SLet name t valExpr

varStmt :: Parser Stmt 
varStmt = do
    name <- reserved "var" >> identifier
    t <- varSygnature 
    valExpr <- reservedOp "=" >> expr
    return $ SVar name t valExpr

letTupleStmt :: Parser Stmt
letTupleStmt = do
    tp <- reserved "let" >> tuple
    t <- varSygnature
    valExpr <- reservedOp "=" >> expr
    return $ SLetTuple tp t valExpr

varTupleStmt :: Parser Stmt
varTupleStmt = do
    tp <- reserved "var" >> tuple
    t <- varSygnature
    valExpr <- reservedOp "=" >> expr
    return $ SVarTuple tp t valExpr

varSygnature :: Parser (Maybe Type)
varSygnature = optionMaybe $ colon >> typeLiteral

funcStmt :: Parser Stmt
funcStmt = SFunction <$> function

printStmt :: Parser Stmt
printStmt = reserved "print" >> (SPrint <$> expr `sepBy1` comma)

flowStmt :: Parser Stmt
flowStmt = ifStmt <|> forStmt <|> whileStmt <|> (returnStmt <* semi)

ifStmt :: Parser Stmt
ifStmt = do
    cond <- reserved "if" >> expr
    trueBlock <- blockStmt
    falseBlock <- optionMaybe $ reserved "else" >> blockStmt
    return $ SIf cond trueBlock falseBlock

forStmt :: Parser Stmt
forStmt = do
    var <- reserved "for" >> identifier
    r <- reserved "in" >> range
    block <- blockStmt
    return $ SFor var r block

whileStmt :: Parser Stmt
whileStmt = do
    cond <- reserved "while" >> expr
    block <- blockStmt
    return $ SWhile cond block

returnStmt :: Parser Stmt
returnStmt = SReturn <$> (reserved "return" >> expr)

postfixStmt :: Parser Stmt
postfixStmt =  
    try (SInc <$> (identifier <* reservedOp "++")) <|>
    (SDec <$> (identifier <* reservedOp "--"))

assignStmt :: Parser Stmt
assignStmt = identifier >>= \var -> choice $ map ($ var)
    [ assignP "=" SEq
    , assignP "+=" SAdd
    , assignP "-=" SSub
    , assignP "*=" SMul
    , assignP "/=" SDiv
    , assignP "%=" SMod ]
    where
    assignP op ctor var = SAssign ctor var <$> (reservedOp op >> expr)

tupleAssignStmt :: Parser Stmt
tupleAssignStmt = do
    tp <- tuple <* reservedOp "="
    e <- expr
    return $ STupleAssign tp e

-- Tuple 

tuple :: Parser Tuple
tuple = Tuple <$> parens (tupleTerm `sepBy2` comma)

tupleTerm :: Parser TupleTerm
tupleTerm = 
    (TupleTermVar <$> identifier) <|>
    (TupleTermTuple <$> tuple)

-- Range

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

-- Types

typeLiteral :: Parser Type
typeLiteral = 
    try functionType <|>
    tupleType <|> 
    simpleType

functionArgType :: Parser Type
functionArgType = 
    simpleType <|> 
    try (parens functionType) <|>
    tupleType

simpleType :: Parser Type
simpleType = choice 
    [ reserved "Int" >> return TInt
    , reserved "Bool" >> return TBool
    , reserved "String" >> return TString ]

functionType :: Parser Type
functionType = TFunc <$> (functionTypeNoArgs <|> functionTypeArgs)

functionTypeArgs :: Parser [Type]
functionTypeArgs = functionArgType `sepBy2` reservedOp "->"

functionTypeNoArgs :: Parser [Type]
functionTypeNoArgs = do
    retT <- reserved "Void" >> reservedOp "->" >> functionArgType
    return [retT]

tupleType :: Parser Type
tupleType = TTuple <$> parens (typeLiteral `sepBy2` comma)

-- Helpers

sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 arg sep = do
    first <- arg <* sep
    rest <- arg `sepBy1` sep
    return $ first:rest
