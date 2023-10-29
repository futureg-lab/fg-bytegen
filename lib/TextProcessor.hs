module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


data FgValue = Literal String
    | List [FgValue]                    -- [v1, v2, ..]
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

parseList :: Parser FgValue
parseList =
        do
            -- TODO: nested & key: value syntax
            char '['
            items <- endBy parseExpr (many space >> many (char ',')) -- <expr> *,
            char ']'
            return $ List items


symbol :: Parser Char
symbol = oneOf "+-/*%<>=(){}"

parseSymbol :: Parser FgValue
parseSymbol =
        let symbolAsString = fmap (:[]) symbol
        in do
            match <- string ".."
                <|> string ">="
                <|> string "<="
                <|> string "=="
                <|> symbolAsString
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
    <|> parseList

readExpr :: String -> String
readExpr input = case parse parseExpr "unexpected token!" input of
    Left err -> "No match: " ++ show err
    Right v -> "Found value " ++ show v

fgParse :: String -> [String]
fgParse x = [x]