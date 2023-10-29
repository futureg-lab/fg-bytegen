module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Text.Parsec.Token (GenTokenParser(charLiteral))
import Data.List.NonEmpty (some1)
import qualified Control.Applicative as Parsec


data FgValue = Literal String
    | Tup [(FgValue, FgValue)]         -- [(k1:)?v1, (k2:)?v2, ..]
    | Number Float                      -- 1234, 1.65 ..
    | String String                     -- "(.+)"
    | Bool Bool                         -- Literal false | true -> Bool false | true
    | Operator String                   -- +, -, /, *, not, in, list generator .., ..
    | Symbol String                     -- (, ), {, }, ..
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


parseNumber :: Parser FgValue
-- equiv. liftM (Number . read) $ many1 digit
parseNumber = Number . read <$> many1 digit


parseTupItem :: Parser (FgValue, FgValue)
parseTupItem =
    let _s = many space in do
    _s
    item <- parseExpr
    _s
    -- try $ string ":"
    -- next_item <- _s >> parseExpr >> _s
    many (char ',')
    return (Number 0, item)

parseTup :: Parser FgValue
parseTup =
        do
            char '['
            items <- many parseTupItem
            char ']'
            return $ Tup items

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