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
                (readExpr "- not not\n[1]")
        ),
        TestCase (assertEqual "parse decimal number" "Number 213.001701" (readExpr "  213.001701")),
        TestCase (assertEqual "empty tup" "Tup []" (readExpr "[\n]")),

        TestCase (
            assertEqual "simple expression"
                "Binary (ListGenerator (Number 1.0) (Binary (OR (Binary (AND (Unary (Negative (Number 2.0))) (Binary (MULT (Number 3.0) (Binary (MINUS (Literal \"x\") (Number 1.0))))))) NullValue)))"
                (readExpr "1 .. -2 and 3 * (x - 1) or null")
        ),

        TestCase (
            assertEqual "parse complex expression"
                "Binary (EQU (Unary (ReprOf (Binary (PLUS (Literal \"x\") (Literal \"y\"))))) (Binary (MULT (Number 5.0) (Unary (Negative (Binary (PLUS (Unary (NOT (Unary (NOT (Unary (NOT (Unary (Negative (Number 4.0))))))))) (Number 8.0))))))))"
                (readExpr "repr_of (x + y) == 5*-(not (not not -4) + 8) ")
        ),

        TestCase (
            assertEqual "nested tup"
                "Tup [(Number 0.0,Number 1.0),(Number 1.0,Tup [(Literal \"a\",Number 2.0),(Literal \"b\",Number 3.0)]),(Number 2.0,String \"three\")]"
                (readExpr "[1,\n [ \na   :2,\n\t\nb  : 3], \"three\"]")
        ),

        TestCase (
            assertEqual "parse root expression"
                "RootExpr (Binary (MULT (Unary (NOT (Binary (MINUS (Literal \"x\") (Number 1.0))))) (Unary (Negative (Number 3.7)))))"
                (readProg " not (x \n- 1) * -\t3.7;")
        ),

        TestCase (
            assertEqual "parse variable declaration"
                "VarDecl (Var {vName = \"my_val\", vType = TypeNative \"auto\"}) (Binary (OR (Binary (OR (Bool False) (Unary (NOT (Bool True))))) (FuncCall \"even\" [Number 1.0])))"
                (readProg " auto my_val = false or not true or even(1) ; ")
        ),

        TestCase (
            assertEqual "parse root block"
                "RootBlock (Block [RootBlock (Block [VarDecl (Var {vName = \"x\", vType = TypeNative \"tup\"}) (Tup [])])])"
                (readProg "{ \n\t{tup   x = [\n];} } ")
        ),

        TestCase (
            assertEqual "parse function declaration"
                "FunDecl {fnName = \"hello\", fnOutType = TypeNative \"auto\", fnArgs = [Var {vName = \"name\", vType = TypeNative \"str\"},Var {vName = \"age\", vType = TypeNative \"num\"}], fnBody = Block []}"
                (readProg " fn  \nhello(\n str name ,\nnum age) -> auto\n;")
        ),

        TestCase (
            assertEqual "parse function declaration with body"
                "FunDecl {fnName = \"aaa\", fnOutType = TypeNative \"auto\", fnArgs = [], fnBody = Block [RootBlock (Block []),Return (Binary (OR (Bool False) (Bool True)))]}"
                (readProg " fn \taaa(\n\n\t)\n ->\t auto\n { {\n\r\n} \nret false or true; }")
        ),

        TestCase (
            assertEqual "parse while loop with control loop, and func calls"
                "WhileLoop (Unary (NOT (FuncCall \"even\" [Binary (MOD (Literal \"x\") (FuncCall \"f\" [Binary (PLUS (Literal \"y\") (Number 5.0))]))]))) (Block [WhileLoop (Unary (NOT (FuncCall \"even\" [Literal \"x\"]))) (Block [LoopContinue]),LoopBreak])"
                (readProg " while\n not\n even(x\n\t % f(y + 5)) {\n  while not even(x) { continue; }\n break; }")
        ),

        TestCase (
            assertEqual "parse for loop with control loop, and func calls"
                "ForLoop {forItem = (Just \"k\",\"v\"), forIterator = Binary (ListGenerator (Number 0.0) (Number 10.0)), forBlock = Block [ForLoop {forItem = (Nothing,\"k2\"), forIterator = Literal \"list\", forBlock = Block [RootExpr (FuncCall \"print\" [Binary (PLUS (Literal \"k\") (Literal \"k2\")),Literal \"v\"])]}]}"
                (readProg " for \t( k ,\tv\t )\n   in 0 \n\t.. 10 {\nfor k2 in list { print(k + k2, v)\n; }\n}")
        )
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess