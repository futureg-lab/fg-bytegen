module FgAST where

data FgValue = Literal String
    | Tup [(FgValue, FgValue)]         -- [(k1:)?v1, (k2:)?v2, ..]
    | Number Double                    -- 1234, 1.65 ..
    | String String                    -- "(.+)"
    | Bool Bool                        -- Literal false | true -> Bool false | true
    | Binary FgBinary
    | Unary FgUnary
    deriving (Show, Eq)

data FgBinary = PLUS FgValue FgValue
    | MINUS FgValue FgValue
    | MULT FgValue FgValue
    | DIV FgValue FgValue
    | EQU FgValue FgValue
    | NEQ FgValue FgValue
    -- | ASSIGN FgValue FgValue
    | GT_ FgValue FgValue
    | GTE FgValue FgValue
    | LT_ FgValue FgValue
    | LTE FgValue FgValue
    | OR FgValue FgValue
    | AND FgValue FgValue
    | XOR FgValue FgValue
    | IN (FgValue, FgValue) FgValue  -- k, v in x
    | ListGenerator FgValue FgValue  -- a .. b
    deriving (Show, Eq)

data FgUnary = NOT FgValue
    | ReprOf FgValue
    | Negative FgValue
    deriving (Show, Eq)

data Operation = FgBinary | FgUnary
    deriving (Show, Eq)