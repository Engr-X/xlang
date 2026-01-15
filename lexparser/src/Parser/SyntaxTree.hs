module SyntaxTree where


data Operator = Add | Minus



data Expression =
    Operation Expression Operator Expression 