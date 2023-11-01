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
lexeme p = whitespace >> p

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

parseNumberPos :: Parser FgValue
parseNumberPos = do
    let decimal = do
            first <- lexeme parseDigits
            char '.'
            rem <- many digit
            let Number n =  first
            let exp = int2Double $ length rem
            let d = read rem
            return $ Number (n + (d / (10 ** exp)))
    try decimal <|> lexeme parseDigits

{- TUPLES/LIST -}

parseTupItem :: Parser (FgValue, FgValue)
parseTupItem = do
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
    items <- lexeme $ try (many parseTupKeyValue) <|> many parseTupItem
    lexeme $ char ']'
    return $ Tup items


{- BINARY OPERATORS -}
-- Grammar X ::= Y <op> X | Y
leftAssociative :: Parser FgValue -> [(FgValue -> FgValue -> FgBinary, String)] -> Parser FgValue
leftAssociative leftParser opAlternatives = do
    let rightExpansion = do
                left <- lexeme leftParser
                op <- lexeme $ choice (map (try . string . snd) opAlternatives)
                right <- leftAssociative leftParser opAlternatives
                -- prepare return value
                let operator = fst $ head (filter ((==) op . snd) opAlternatives)
                return $ Binary $ operator left right
    try rightExpansion <|> lexeme leftParser

-- gen_expr  ::= or_expr (..) gen_expr | or_expr
parseGenExpr :: Parser FgValue
parseGenExpr = leftAssociative parseOr [(ListGenerator, "..")]

-- or_expr   ::= xor_expr ( xor ) or_expr | xor_expr
parseOr :: Parser FgValue
parseOr = leftAssociative parserXOR [(OR, "or ")]

-- xor_expr  ::= and_expr ( or ) xor_expr | and_expr
parserXOR :: Parser FgValue
parserXOR = leftAssociative parserAND [(XOR, "xor ")]

-- and_expr  ::= comp_expr ( and ) and_expr | expr
parserAND :: Parser FgValue
parserAND = leftAssociative parserCompExpr [(AND, "and ")]

-- comp_expr ::= expr (== | >= | >= | != | < | >) comp_expr | expr
parserCompExpr :: Parser FgValue
parserCompExpr = leftAssociative parseSimpleExpr [
        (LTE, "<="), (LT_, "<"),
        (GTE, ">="), (GT_ ,">"),
        (NEQ, "!="), (EQU, "==")
    ]

-- sexpr     ::= term (+| -) sexpr | term
parseSimpleExpr :: Parser FgValue
parseSimpleExpr = leftAssociative parseTerm [(PLUS, "+"), (MINUS, "-")]

-- term      ::= factor (* | /) term | factor
parseTerm :: Parser FgValue
parseTerm = leftAssociative parseFactor [(MULT, "*"), (DIV, "/")]


parseParenth :: Parser FgValue
parseParenth = do
    lexeme (char '(')
    expr <- lexeme parseGenExpr
    lexeme (char ')')
    return expr

parseFactor :: Parser FgValue
parseFactor = try parseParenth <|> parseUnary



{- UNARY OPERATORS / OPERANDS -}

parseUnarySpacedOp :: (FgValue -> FgUnary) -> String -> Parser FgValue
parseUnarySpacedOp op tk = string tk >> many1 space >> Unary . op <$> parseFactor

parseUnaryNegative :: Parser FgValue
parseUnaryNegative = char '-' >> many space >> Unary . Negative <$> parseFactor

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
parseExpr = lexeme (parseParenth <|> try parseGenExpr <|> parseUnary)


gen :: Parser FgValue -> String -> String
gen p input = case parse p "unexpected token!" input of
    Left err -> show err
    Right v -> show v

readExpr :: String -> String
readExpr = gen parseExpr