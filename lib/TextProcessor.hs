module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Text.Parsec.Token (GenTokenParser(charLiteral, decimal))
import Data.List.NonEmpty (some1)
import qualified Control.Applicative as Parsec
import qualified Data.Foldable as Parsec
import GHC.Float


data FgValue = Literal String
    | Tup [(FgValue, FgValue)]         -- [(k1:)?v1, (k2:)?v2, ..]
    | Number Double                    -- 1234, 1.65 ..
    | String String                    -- "(.+)"
    | Bool Bool                        -- Literal false | true -> Bool false | true
    | Operator String                  -- +, -, /, *, not, in, list generator .., ..
    | Symbol String                    -- (, ), {, }, ..
    deriving (Show, Eq)

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

{- SYMBOLS/OPERATORS -}

symbol :: Parser Char
symbol = oneOf ".+-/*%<>=(){}"


parseSymbol :: Parser FgValue
parseSymbol =
        let symbolAsString = fmap (:[]) symbol
        in do
            match <- try (
                        string ".."
                    <|> string ">="
                    <|> string "<="
                    <|> string "=="
                ) <|> symbolAsString
            -- TODO: refactor
            return $ case match of
                ".." -> Operator ".."
                "==" -> Operator "=="
                "+" -> Operator "+"
                "-" -> Operator "-"
                "/" -> Operator "/"
                "*" -> Operator "*"
                "=" -> Operator "="
                "%" -> Operator "%"
                ">=" -> Operator ">="
                ">" -> Operator ">"
                "<=" -> Operator "<="
                "<" -> Operator "<"
                s -> Symbol s


parseExpr :: Parser FgValue
parseExpr = parseLiteral
    <|> parseString
    <|> parseNumber
    <|> parseSymbol
    <|> parseTup


readExpr :: String -> String
readExpr input = case parse parseExpr "unexpected token!" input of
    Left err -> show err
    Right v -> show v


fgParse :: String -> [String]
fgParse x = [x]