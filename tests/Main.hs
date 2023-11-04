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
                "Tup [(Number 0.0,Number 1.0),(Number 1.0,Tup [(Literal \"a\",Number 2.0),(Literal \"b\",Number 3.0)]),(Number 2.0,String \"three\")]"
                (readExpr "[1, [ a   :2,b  : 3], \"three\"]")
        ),

        TestCase (
            assertEqual "parse root expression"
                "RootExpr (Binary (MULT (Unary (NOT (Binary (MINUS (Literal \"x\") (Number 1.0))))) (Unary (Negative (Number 3.7)))))"
                (readProg " not (x - 1) * -3.7;")
        ),

        TestCase (
            assertEqual "parse variable declaration"
                "VarDecl (Var {vName = \"my_val\", vType = TypeNative \"auto\"}) (Binary (OR (Bool False) (Unary (NOT (Bool True)))))"
                (readProg " auto my_val = false or not true  ; ")
        ),

        TestCase (
            assertEqual "parse root block"
                "RootBlock (Block [RootBlock (Block [VarDecl (Var {vName = \"x\", vType = TypeNative \"tup\"}) (Tup [])])])"
                (readProg "{ {tup   x = [];} } ")
        ),

        TestCase (
            assertEqual "parse function declaration"
                "FunDecl {fnName = \"hello\", fnOutType = TypeNative \"auto\", fnArgs = [Var {vName = \"name\", vType = TypeNative \"str\"},Var {vName = \"age\", vType = TypeNative \"num\"}], fnBody = Block []}"
                (readProg " fn  hello(str name, num age) -> auto;")
        ),

        TestCase (
            assertEqual "parse function declaration with body"
                "FunDecl {fnName = \"aaa\", fnOutType = TypeNative \"auto\", fnArgs = [], fnBody = Block [RootBlock (Block []),Return (Binary (OR (Bool False) (Bool True)))]}"
                (readProg " fn aaa() -> auto { {} ret false or true; }")
        )
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess