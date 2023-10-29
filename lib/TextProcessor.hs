module TextProcessor where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


data FgValue = Literal String
    | List [FgValue]                    -- [v1, v2, ..]
    | ListGenerator FgValue FgValue     -- v1 .. v2
    | Number Float                      -- 1234, 1.65 ..
    | String String                     -- "(.+)"
    | Bool Bool                         -- Literal false | true -> Bool false | true
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
        start <- letter <|> underscore -- cannot start with a number
        rest <- many (letter <|> digit <|> underscore)
        let matched = start:rest
        return $ case matched of
            "false" -> Bool False
            "true"  -> Bool True
            _       -> Literal matched


parseNumber :: Parser FgValue
-- liftM (Number . read) $ many1 digit
parseNumber = Number . read <$> many1 digit


parseExpr :: Parser FgValue
parseExpr = parseLiteral
    <|> parseString
    <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "unexpected token!" input of
    Left err -> "No match: " ++ show err
    Right v -> "Found value " ++ show v

fgParse :: String -> [String]
fgParse x = [x]