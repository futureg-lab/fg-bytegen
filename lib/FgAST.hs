module FgAST where

data FgValue = Literal String
    | Tup [(FgValue, FgValue)]         -- [(k1:)?v1, (k2:)?v2, ..]
    | Number Double                    -- 1234, 1.65 ..
    | String String                    -- "(.+)"
    | Bool Bool                        -- Literal false | true -> Bool false | true
    | Binary FgBinary
    | Unary FgUnary
    | NullValue
    | FuncCall String [FgValue]
    deriving (Show, Eq)

data FgBinary = PLUS FgValue FgValue
    | MINUS FgValue FgValue
    | MULT FgValue FgValue
    | DIV FgValue FgValue
    | EQU FgValue FgValue
    | NEQ FgValue FgValue
    | ASSIGN FgValue FgValue
    | GT_ FgValue FgValue
    | GTE FgValue FgValue
    | LT_ FgValue FgValue
    | LTE FgValue FgValue
    | OR FgValue FgValue
    | AND FgValue FgValue
    | XOR FgValue FgValue
    | MOD FgValue FgValue
    | ListGenerator FgValue FgValue  -- a .. b
    deriving (Show, Eq)

data FgUnary = NOT FgValue
    | ReprOf FgValue
    | Negative FgValue
    deriving (Show, Eq)

newtype FgType = TypeNative String
    deriving (Show, Eq)

newtype FgBlock = Block [FgInstr]
    deriving (Show, Eq)

data FgVariable = Var { vName :: String, vType :: FgType }
    deriving (Show, Eq)


data FgInstr = RootExpr FgValue
    | RootBlock FgBlock
    | Return FgValue
    | VarDecl FgVariable FgValue
    | WhileLoop FgValue FgBlock
    | ForLoop {
         forItem:: (Maybe String, String)
        ,forIterator :: FgValue
        ,forBlock :: FgBlock
    }
    | FunDecl {
             fnName :: String
            ,fnOutType :: FgType
            ,fnArgs :: [FgVariable]
            ,fnBody :: FgBlock
        }
    | LoopBreak
    | LoopContinue
    deriving (Show, Eq)
