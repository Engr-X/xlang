module Parse.Parser where

data Expression = 
    IntConst String | -- For precesion use String to store rather than Integer
    LongConst String |
    FloatConst String |
    DoubleConst String |
    LongDoubleConst String | 

    CharConst Char | -- the ' is not include
    StringConst String | -- the " is not include
    BoolConst Bool | 
    Add Expression Expression |
    Substract Expression Expression



