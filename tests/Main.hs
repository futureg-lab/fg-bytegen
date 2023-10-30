module Main where

import TextProcessor
import Test.HUnit
import qualified System.Exit as Exit


tests :: Test
tests = TestList [
        TestCase (assertEqual "parse string" "String \"Hello\"" (readExpr "\"Hello\"")),
        TestCase (assertEqual "symbol: parse dot" "Symbol \".\"" (readExpr ".")),
        TestCase (assertEqual "symbol: parse double dot" "Operator \"..\"" (readExpr "..")),
        TestCase (assertEqual "parse natural numbeer" "Number 1234.0" (readExpr "1234")),
        TestCase (assertEqual "parse natural numbeer" "Number \"1234.00701\"" (readExpr "1234.00701")),
        TestCase (assertEqual "empty tup" "Tup []" (readExpr "[]")),
        TestCase (
            assertEqual "nested tup"
                "Tup [(Number 0.0,Number 1.0),(Number 0.0,Tup [(Literal \"a\",Number 2.0),(Literal \"b\",Number 3.0)]),(Number 0.0,String \"three\")]"
                (readExpr "[1, [ a   :2,b  : 3], \"three\"]")
        )
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess