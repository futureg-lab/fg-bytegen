module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List.NonEmpty (some1)
import qualified Control.Applicative as Parsec
import qualified Data.Foldable as Parsec
import GHC.Float

import FgAST
import Control.Monad

whitespace :: Parser ()
whitespace = void $ many space

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

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
        _       -> Literal literal


{- NUMBERS -}

parseDigits :: Parser FgValue
-- equiv. liftM (Number . read) $ many1 digit
parseDigits = Number . read <$> lexeme (many1 digit)

parseFloat :: Parser FgValue
parseFloat = do
    first <- parseDigits
    char '.'
    rem <- many digit
    let Number n =  first
    let exp = int2Double $ length rem
    let d = read rem
    return $ Number (n + (d / (10 ** exp)))

parseNumber :: Parser FgValue
parseNumber = do
    let neg = do
            char '-'
            Number x <- positiveNumber
            return $ Number (-x)
    try neg <|> try positiveNumber
    where
        positiveNumber = try parseFloat <|> parseDigits


{- TUPLES/LIST -}

parseTupItem :: Parser (FgValue, FgValue)
parseTupItem = let _s = whitespace in do
    item <- lexeme parseExpr
    lexeme $ many (char ',')
    return (Number 0, item)


parseTupKeyValue :: Parser (FgValue, FgValue)
parseTupKeyValue = do
    key <- lexeme parseExpr
    lexeme (char ':')
    value <- lexeme parseExpr
    lexeme $ many (char ',')
    return (key, value)


parseTup :: Parser FgValue
parseTup = do
    lexeme $ char '['
    items <- try (many parseTupKeyValue) <|> many parseTupItem
    lexeme $ char ']'
    return $ Tup items


{- BINARY OPERATORS -}
-- Grammar X ::= Y <op> X | Y
leftAssociative :: Parser FgValue -> (FgValue -> FgValue -> FgBinary) -> Parser a -> Parser FgValue
leftAssociative leftParser op token = do
    let rightExpansion = do
            left <- lexeme leftParser
            lexeme token
            right <- lexeme parseGenExpr
            return $ Binary $ op left right
    try rightExpansion <|> lexeme (try leftParser)

-- gen_expr  ::= or_expr (..) gen_expr | or_expr
parseGenExpr :: Parser FgValue
parseGenExpr = leftAssociative parseSimpleExpr ListGenerator (string "..")

-- or_expr   ::= xor_expr (xor) or_expr | xor_expr
parseOr :: Parser FgValue
parseOr = leftAssociative parserXOR OR (string "or")

-- xor_expr  ::= and_expr (or) xor_expr | and_expr
parserXOR :: Parser FgValue
parserXOR = leftAssociative parserAND XOR (string "xor")

-- and_expr  ::= comp_expr (and) and_expr | expr
parserAND :: Parser FgValue
parserAND = leftAssociative parserCompExpr AND (string "and")

-- comp_expr ::= expr (== | >= | >= | != | < | >) comp_expr | expr
parserCompExpr :: Parser FgValue
parserCompExpr = -- WHY ???? are <, >, = sepcials?
        bin LTE (string "<=") <|> bin LT_ (char '<')
    <|> bin GTE (string ">=") <|> bin GT_ (char '>')
    <|> bin NEQ (string "!=")
    <|> bin EQU (string "==")
    where bin = leftAssociative parseSimpleExpr

-- sexpr     ::= term (+| -) sexpr | term
parseSimpleExpr :: Parser FgValue
parseSimpleExpr =
        bin PLUS (char '+')
    <|> bin MINUS (char '-')
    where bin = leftAssociative parseTerm

-- term      ::= factor (* | /) term | factor
parseTerm :: Parser FgValue
parseTerm =
        bin MULT (char '*')
    <|> bin DIV (char '/')
    where bin = leftAssociative parseFactor




parseParenth :: Parser FgValue
parseParenth = do
    lexeme (char '(')
    expr <- parseGenExpr
    lexeme (char ')')
    return expr

parseFactor :: Parser FgValue
parseFactor = try parseParenth <|> parseUnary



{- UNARY OPERATORS / OPERANDS -}

parseUnarySpacedOp :: (FgValue -> FgUnary) -> String -> Parser FgValue
parseUnarySpacedOp op tk = string tk >> space >> Unary . op <$> parseFactor

parseUnaryNegative :: Parser FgValue
parseUnaryNegative = char '-' >> many space >> Unary . Negative <$> parseFactor

parseUnary :: Parser FgValue
parseUnary = lexeme $ parseUnarySpacedOp ReprOf "repr_of"
    <|> parseUnarySpacedOp NOT "not"
    <|> parseUnaryNegative
    <|> parseLiteral
    <|> parseString
    <|> parseNumber
    <|> parseTup

{- EXPRESSION -}
parseExpr :: Parser FgValue
parseExpr = try whitespace >> (
            try parseParenth
        <|> try parseGenExpr
        <|> parseUnary
    )

readExpr :: String -> String
readExpr input = case parse parseExpr "unexpected token!" input of
    Left err -> show err
    Right v -> show v


fgParse :: String -> [String]
fgParse x = [x]