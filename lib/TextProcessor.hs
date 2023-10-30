module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Text.Parsec.Token (GenTokenParser(charLiteral, decimal))
import Data.List.NonEmpty (some1)
import qualified Control.Applicative as Parsec
import qualified Data.Foldable as Parsec
import GHC.Float

import FgAST

parseString :: Parser FgValue
parseString =
        do
            char '"'
            x <- many (noneOf escaped)
            char '"'
            return $ String x
        where
            escaped = "\""

parseLiteral :: Parser FgValue
parseLiteral =
    let underscore = char '_' in do
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
parseDigits = Number . read <$> many1 digit

parseFloat :: Parser FgValue
parseFloat =
    do
        first <- parseDigits
        char '.'
        rem <- many digit
        let Number n =  first
        let exp = int2Double $ length rem
        let d = read rem
        return $ Number (n + (d / (10 ** exp)))

parseNumber :: Parser FgValue
parseNumber =
    do
        let neg = do
                char '-'
                Number x <- positiveNumber
                return $ Number (-x)
        try neg <|> try positiveNumber
    where
        positiveNumber = try parseFloat <|> parseDigits


{- TUPLES/LIST -}

parseTupItem :: Parser (FgValue, FgValue)
parseTupItem =
    let _s = many space in do
    _s
    item <- parseExpr
    _s
    -- option (char ',') ?? 
    many (char ',')
    return (Number 0, item)


parseTupKeyValue :: Parser (FgValue, FgValue)
parseTupKeyValue =
    let _s = many space in do
    _s
    key <- parseExpr
    _s >> char ':' >> _s
    value <- parseExpr
    _s >> many (char ',')
    return (key, value)


parseTup :: Parser FgValue
parseTup =
        do
            char '['
            items <- try (many parseTupKeyValue) <|> many parseTupItem
            char ']'
            return $ Tup items


{- BINARY OPERATORS -}
-- Grammar X ::= Y <op> X | Y
parseBinaryOp :: Parser FgValue -> (FgValue -> FgValue -> FgBinary) -> String -> Parser FgValue
parseBinaryOp leftParser op tk  = do
    let expand = do
            left <- leftParser
            many space
            string tk
            many space
            right <- parseBinaryOp leftParser op tk
            return $ Binary (op left right)
    try expand <|> leftParser

-- gen_expr  ::= or_expr (..) gen_expr | or_expr
parseGenExpr :: Parser FgValue
parseGenExpr = parseBinaryOp parseOr ListGenerator ".."

-- or_expr   ::= xor_expr (xor) or_expr | xor_expr
parseOr :: Parser FgValue
parseOr = parseBinaryOp parserXOR OR "or"

-- xor_expr  ::= and_expr (or) xor_expr | and_expr
parserXOR :: Parser FgValue
parserXOR = parseBinaryOp parserAND XOR "xor"

-- and_expr  ::= comp_expr (and) and_expr | expr
parserAND :: Parser FgValue
parserAND = parseBinaryOp parserCompExpr AND "and"

-- comp_expr ::= expr (== | >= | >= | != | < | >) comp_expr | expr
parserCompExpr :: Parser FgValue
parserCompExpr = -- WHY ???? are <, >, = sepcials?
        try (bin LTE "<=") <|> bin LT_ "<"
    <|> try (bin GTE ">=") <|> bin GT_ ">"
    <|> bin NEQ "!="
    <|> bin EQU "=="
    where bin = parseBinaryOp parseSimpleExpr

-- sexpr     ::= term (+| -) sexpr | term
parseSimpleExpr :: Parser FgValue
parseSimpleExpr =
        bin PLUS "+"
    <|> bin MINUS "-"
    where bin = parseBinaryOp parseTerm

-- term      ::= factor (* | /) term | factor
parseTerm :: Parser FgValue
parseTerm =
        bin MULT "*"
    <|> bin DIV "/"
    where bin = parseBinaryOp parseFactor

-- factor    ::= (gen_expr) | unary
parseFactor :: Parser FgValue
parseFactor =
    do
    let parenth = do
            char '('
            many space
            expr <- parseGenExpr
            many space
            char ')'
            return expr
    parenth <|> parseUnary


parseBinary :: Parser FgValue
parseBinary = parseFactor

{- UNARY OPERATORS / OPERANDS -}

parseUnaryOp :: (FgValue -> FgUnary) -> String -> Parser FgValue
parseUnaryOp op tk = string tk >> many space >> Unary . op <$> parseExpr

parseUnary :: Parser FgValue
parseUnary = parseUnaryOp ReprOf "repr_of"
    <|> parseUnaryOp NOT "not"
    <|> parseUnaryOp Negative "-"
    <|> parseLiteral
    <|> parseString
    <|> parseNumber
    <|> parseTup

{- EXPRESSION -}
parseExpr :: Parser FgValue
parseExpr = parseFactor <|> parseUnary
-- parseExpr = parseUnary



readExpr :: String -> String
readExpr input = case parse parseExpr "unexpected token!" input of
    Left err -> show err
    Right v -> show v


fgParse :: String -> [String]
fgParse x = [x]