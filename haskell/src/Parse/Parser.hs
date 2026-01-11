module Parse.Parser where

data Class = Int8T | Int16T | Int32T | Int64T |
             Float16T | Float32T | Float64T | Float128T |
             Bool | Char | 
             Array Class Int |
             Function [Class]

data Command = Continue | Break | Return (Maybe Expression)

data Operator = Add -- Todo

data Operate = Class Expression Operator Expression

data Expression = 
    Empty |
    Command Command |
    IntConst String | -- For precesion use String to store rather than Integer
    LongConst String |
    FloatConst String |
    DoubleConst String |
    LongDoubleConst String | 

    CharConst Char | -- the ' is not include
    StringConst String | -- the " is not include
    BoolConst Bool |

    Variable Class String |

    Operate Operate

data Block = Single Statement | Multiple [Statement] |
    Case Int Block | Default Block

data Statement = 
    Call Expression |
    If Expression Block Block |
    For (Expression, Expression, Expression) Block |
    While Expression Block |
    Switch Expression [Block]


isEmptyExpr :: Expression -> Bool
isEmptyExpr Empty = True
isEmptyExpr  _ = False