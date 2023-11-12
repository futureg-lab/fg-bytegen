{-# LANGUAGE FlexibleContexts #-}
module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import GHC.Float
import Text.Parsec.Expr
import Control.Monad
import Text.Parsec (Parsec)

import FgAST

whitespace :: Parser ()
whitespace = void $ many space

lexeme :: Parser a -> Parser a
lexeme p = whitespace >> p

lexemeVoid :: Parser a -> Parser ()
lexemeVoid = void . lexeme

parseString :: Parser FgValue
parseString = do
    lexemeVoid $ char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseLiteral :: Parser FgValue
parseLiteral = let underscore = char '_' in do
    head <- letter <|> underscore -- cannot start with a number
    tail <- many (letter <|> digit <|> underscore)
    let literal = head:tail
    return $ case literal of
        "false" -> Bool False
        "true"  -> Bool True
        "null"  -> NullValue
        _       -> Literal literal


{- NUMBERS -}

parseDigits :: Parser FgValue
-- equiv. liftM (Number . read) $ many1 digit
parseDigits = Number . read <$> lexeme (many1 digit)

parseNumberPos :: Parser FgValue
parseNumberPos = do
    let decimal = do
            first <- lexeme (many1 digit)
            void $ char '.'
            second <- many1 digit
            return $ (Number . read) (first ++ "." ++ second)
    try decimal <|> lexeme parseDigits



{- TUPLES/LIST -}

parseTupSimple :: Parser FgValue
parseTupSimple = do
    let v = do
            item <- lexeme parseExpr
            void whitespace
            return (Number 0, item)
    lexemeVoid $ char '['
    whitespace
    items <- lexeme $ sepBy v (char ',')
    whitespace
    lexemeVoid $ char ']'
    return $ Tup [(Number (int2Double n), snd (items !! n))| n <- [0 .. length items - 1]]

parseTupKeyValue :: Parser FgValue
parseTupKeyValue = do
    let kv = do
            key <- lexeme (parseLiteral <|> parseString <|> parseDigits)
            lexeme (char ':')
            value <- lexeme parseExpr
            void whitespace
            return (key, value)
    lexemeVoid $ char '['
    items <- lexeme $ sepBy kv (char ',')
    lexemeVoid $ char ']'
    return $ Tup items

parseTup :: Parser FgValue
parseTup = try parseTupKeyValue <|> parseTupSimple

parseTupIndexAccess :: Parser FgValue
parseTupIndexAccess = do
    name <- fromLiteral <$> parseLiteral
    lexemeVoid $ char '['
    items <- lexeme $ sepBy parseExpr (char ',')
    lexemeVoid $ char ']'
    return $ TupIndexAccess name items

{- FUNC CALL -}

parseFuncCall :: Parser FgValue
parseFuncCall = do
    let argUnit = do
            item <- lexeme parseExpr
            void whitespace
            return item
    callee <- fromLiteral <$> lexeme parseLiteral
    lexemeVoid $ char '('
    args <- sepBy (try argUnit) (char ',')
    lexemeVoid $ char ')'
    return $ FuncCall callee args


{- BINARY OPERATORS -}

parseGenExpr :: Parsec String () FgValue
parseGenExpr = buildExpressionParser [
       [binary MULT "*" AssocLeft]
      ,[binary DIV "/" AssocLeft]
      ,[binary MOD "%" AssocLeft]
      ,[binary PLUS "+" AssocLeft]
      ,[binary MINUS "-" AssocLeft]
      ,[binary AND "and" AssocLeft]
      ,[binary OR "or" AssocLeft]
      ,[binary EQU "==" AssocLeft]
      ,[binary EQU "is" AssocLeft]
      ,[binary LTE "<=" AssocLeft]
      ,[binary LT_ "<" AssocLeft]
      ,[binary GTE ">=" AssocLeft]
      ,[binary GT_ ">" AssocLeft]
      ,[binary ListGenerator ".." AssocLeft]
      ,[binary ASSIGN "=" AssocLeft]
   ] (lexeme parseFactor)
   where
        binOp op x y = Binary $ op x y
        binary op c = Infix (do
                void $ try $ string c -- backtrack
                return $ binOp op
            )

parseParenth :: Parser FgValue
parseParenth = do
    lexeme (char '(')
    expr <- parseExpr
    lexeme (char ')')
    return expr


parseFactor :: Parser FgValue
parseFactor = do
    ret <- try parseParenth <|> parseUnary
    whitespace -- binOp patch
    return ret



{- EXPRESSION -}

parseUnarySpacedOp :: (FgValue -> FgUnary) -> String -> Parser FgValue
parseUnarySpacedOp op tk = do
    string tk
    many1 space
    lexeme $ Unary . op <$> parseFactor

parseUnaryNegative :: Parser FgValue
parseUnaryNegative = do
    char '-'
    whitespace
    lexeme $ Unary . Negative <$> parseFactor

parseUnary :: Parser FgValue
parseUnary = try (parseUnarySpacedOp ReprOf "repr_of")
    <|> try (parseUnarySpacedOp NOT "not")
    <|> try parseFuncCall
    <|> try parseTupIndexAccess
    <|> parseUnaryNegative
    <|> parseLiteral
    <|> parseString
    <|> parseNumberPos
    <|> parseTup

parseExpr :: Parser FgValue
parseExpr = lexeme parseGenExpr



{- INSTRUCTION -}

terminalSymb :: Parser ()
terminalSymb = lexeme (void $ many1 (char ';'))

parseRootExpr :: Parser FgInstr
parseRootExpr = do
    expr <- lexeme parseExpr
    terminalSymb
    return $ RootExpr expr

parseRootBlock :: Parser FgInstr
parseRootBlock = do
    block <- lexeme parseBlock
    return $ RootBlock block

parseReturn :: Parser FgInstr
parseReturn = do
    lexemeVoid $ string "ret"
    expr <- lexeme parseExpr;
    terminalSymb
    return $ Return expr

fromLiteral :: FgValue -> String
fromLiteral (Literal x) = x
fromLiteral _ = error "fatal: failed unwrapping non-literal token"

parseType :: Parser FgType
parseType = do
    lit <- fromLiteral <$> lexeme parseLiteral
    return $ TypeNative lit

parseVariable :: Parser FgVariable
parseVariable = do
    tpe <- lexeme parseType
    many1 $ char ' '
    varName <- fromLiteral <$> lexeme parseLiteral
    return $ Var { vName=varName, vType=tpe }

parseVariableDecl :: Parser FgInstr
parseVariableDecl = do
    var <- lexeme parseVariable
    lexemeVoid $ char '='
    expr <- parseExpr
    terminalSymb
    return $ VarDecl var expr

parseBlock :: Parser FgBlock
parseBlock = do
    lexemeVoid $ char '{'
    instrs <- many (try parseInstruction)
    lexemeVoid $ char '}'
    return $ Block instrs

parseFunDecl :: Parser FgInstr
parseFunDecl = do
    -- func name
    lexemeVoid $ string "fn"
    many1 $ char ' '
    lit <- fmap fromLiteral (lexeme parseLiteral)
    -- args
    let argUnit = do
            item <- lexeme parseVariable
            void whitespace
            return item
    lexemeVoid $ char '('
    args <- lexeme $ sepBy argUnit (char ',')
    lexemeVoid $ char ')'
    -- func output
    lexemeVoid $ string "->"
    outType <- lexeme parseType
    -- func body
    let emptyBody = do
            terminalSymb
            return $ Block []
    body <- try parseBlock <|> emptyBody
    return $ FunDecl {
         fnName=lit
        ,fnOutType=outType
        ,fnArgs=args
        ,fnBody=body
    }

parseContinue :: Parser FgInstr
parseContinue = lexemeVoid (string "continue") >> terminalSymb >> return LoopContinue

parseBreak :: Parser FgInstr
parseBreak =  lexemeVoid (string "break") >> terminalSymb >> return LoopBreak

parseWhileLoop :: Parser FgInstr
parseWhileLoop = do
    lexemeVoid $ string "while"
    cond <- lexeme parseExpr
    body <- lexeme parseBlock
    return $ WhileLoop cond body

parseForLoop :: Parser FgInstr
parseForLoop = do
    let parenthItem = do
            lexemeVoid $ char '('
            k <- fromLiteral <$> lexeme parseLiteral
            lexemeVoid $ char ','
            v <- fromLiteral <$> lexeme parseLiteral
            lexemeVoid $ char ')'
            return (Just k, v)
    let simpleItem  = do
            v <- fromLiteral <$> lexeme parseLiteral
            return (Nothing, v)
    lexemeVoid $ string "for"
    (k, v) <- try parenthItem <|> simpleItem
    lexemeVoid $ string "in"
    iterator <- lexeme parseExpr
    body <- lexeme parseBlock
    return $ ForLoop {
                 forItem=(k, v)
                ,forIterator=iterator
                ,forBlock=body
            }

parseIfStmt :: Parser FgInstr
parseIfStmt = do
    lexemeVoid $ string "if"
    ifCond <- lexeme parseExpr
    ifBody <- lexeme parseBlock
    let expandElif = do
            lexemeVoid $ string "elif"
            elifCond <- lexeme parseExpr
            elifBody <- lexeme parseBlock
            return (elifCond, elifBody)
    elifs <- many (try expandElif)
    let withElse = do
            try $ lexemeVoid $ string "else"
            els <- lexeme parseBlock
            return $ IfStmt {
                ifBranch=(ifCond, ifBody)
                ,elifBranches=elifs
                ,elseBranch=Just els
            }
    let withoutElse = do
            return $ IfStmt {
                ifBranch=(ifCond, ifBody)
                ,elifBranches=elifs
                ,elseBranch=Nothing
            }
    try withElse <|> withoutElse

parseInstruction = try parseRootBlock
    <|> try parseContinue
    <|> try parseBreak
    <|> try parseVariableDecl
    <|> try parseFunDecl
    <|> try parseWhileLoop
    <|> try parseIfStmt
    <|> try parseForLoop
    <|> try parseReturn
    <|> parseRootExpr

parseProgram :: Parser FgInstr
parseProgram = parseInstruction

gen :: Parser FgValue -> String -> String
gen p input = case parse p "unexpected token!" input of
    Left err -> show err
    Right v -> show v
readExpr :: String -> String
readExpr = gen parseExpr

readProg :: String -> String
readProg input = case parse parseProgram "unexpected token!" input of
    Left err -> show err
    Right v -> show v
