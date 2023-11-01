module Main where

import TextProcessor
import Test.HUnit
import qualified System.Exit as Exit


tests :: Test
tests = TestList [
        TestCase (assertEqual "parse string" "String \"Hello\"" (readExpr "\"Hello\"  ")),
        TestCase (assertEqual "parse simple number" "Unary (Negative (Unary (Negative (Tup []))))" (readExpr " - - [  ]")),
        TestCase (
            assertEqual "parse unary operator"
                "Unary (Negative (Unary (NOT (Unary (NOT (Tup [(Number 0.0,Number 1.0)]))))))"
                (readExpr "- not not [1]")
        ),
        TestCase (assertEqual "parse decimal number" "Number 213.001701" (readExpr "  213.001701")),
        TestCase (assertEqual "empty tup" "Tup []" (readExpr "[]")),
        TestCase (
            assertEqual "nested tup"
                "Tup [(Number 0.0,Number 1.0),(Number 0.0,Tup [(Literal \"a\",Number 2.0),(Literal \"b\",Number 3.0)]),(Number 0.0,String \"three\")]"
                (readExpr "[1, [ a   :2,b  : 3], \"three\"]")
        ),

        TestCase (
            assertEqual "simple expression"
                "Binary (ListGenerator (Number 1.0) (Binary (AND (Unary (Negative (Number 2.0))) (Binary (MULT (Number 3.0) (Binary (MINUS (Literal \"x\") (Number 1.0))))))))"
                (readExpr "1 .. -2 and 3 * (x - 1)")
        ),

        TestCase (
            assertEqual "parse complex expression"
                "Binary (EQU (Unary (ReprOf (Binary (PLUS (Literal \"x\") (Literal \"y\"))))) (Binary (MULT (Number 5.0) (Unary (Negative (Binary (PLUS (Unary (NOT (Unary (NOT (Unary (NOT (Unary (Negative (Number 4.0))))))))) (Number 8.0))))))))"
                (readExpr "repr_of (x + y) == 5*-(not (not not -4) + 8) ")
        ),

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