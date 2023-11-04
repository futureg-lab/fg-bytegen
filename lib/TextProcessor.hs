{-# LANGUAGE FlexibleContexts #-}
module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import GHC.Float
import Text.Parsec.Expr

import FgAST


import Control.Monad
import Text.Parsec (Parsec)

whitespace :: Parser ()
whitespace = void $ many space

lexeme :: Parser a -> Parser a
lexeme p = whitespace >> p

constant :: String -> Parser String
constant = lexeme . string

parseString :: Parser FgValue
parseString = do
    lexeme $ char '"'
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
        _       -> Lit $ Literal literal


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
            return (Number 0, item)
    lexeme $ char '['
    items <- lexeme $ sepBy v (char ',')
    lexeme $ char ']'
    return $ Tup [(Number (int2Double n), snd (items !! n))| n <- [0 .. length items - 1]]

parseTupKeyValue :: Parser FgValue
parseTupKeyValue = do
    let kv = do
            key <- lexeme (parseLiteral <|> parseString <|> parseDigits)
            lexeme (char ':')
            value <- lexeme parseExpr
            return (key, value)
    lexeme $ char '['
    items <- lexeme $ sepBy kv (char ',')
    lexeme $ char ']'
    return $ Tup items

parseTup :: Parser FgValue
parseTup = try parseTupKeyValue <|> parseTupSimple

{- BINARY OPERATORS -}

parseGenExpr :: Parsec String () FgValue
parseGenExpr = buildExpressionParser [
       [binary MULT "*" AssocLeft]
      ,[binary DIV "/" AssocLeft]
      ,[binary PLUS "+" AssocLeft]
      ,[binary MINUS "-" AssocLeft]
      ,[binary AND "and" AssocLeft]
      ,[binary OR "or" AssocLeft]
      ,[binary EQU "==" AssocLeft]
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



{- UNARY OPERATORS / OPERANDS -}

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
parseUnary = parseUnarySpacedOp ReprOf "repr_of"
    <|> parseUnarySpacedOp NOT "not"
    <|> parseUnaryNegative
    <|> parseLiteral
    <|> parseString
    <|> parseNumberPos
    <|> parseTup

{- EXPRESSION -}
parseExpr :: Parser FgValue
parseExpr = try (lexeme parseGenExpr) <|> parseUnary


gen :: Parser FgValue -> String -> String
gen p input = case parse p "unexpected token!" input of
    Left err -> show err
    Right v -> show v

readExpr :: String -> String
readExpr = gen parseExpr