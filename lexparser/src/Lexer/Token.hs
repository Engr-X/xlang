module Lexer.Token where

import Util.Types


-- | Symbol tokens for operators and delimiters in the language.
--
-- Each constructor corresponds to a string in 'symbols'.
data Symbol
    -- Assignment operators
    = Assign           -- "="
    | BitLShiftAssign  -- "<<="
    | BitRShiftAssign  -- ">>="
    | BitOrAssign      -- "|="
    | BitXorAssign     -- "^="
    | BitXnorAssign    -- "!^="
    | PlusAssign       -- "+="
    | MinusAssign      -- "-="
    | MultiplyAssign   -- "*="
    | DivideAssign     -- "/="
    | ModuloAssign     -- "%="
    | PowerAssign      -- "**="

    -- Equality / comparison
    | Equal            -- "=="
    | NotEqual         -- "!="
    | GreaterThan      -- ">"
    | LessThan         -- "<"
    | GreaterEqual     -- ">="
    | LessEqual        -- "<="

    -- Bitwise operators
    | BitRShift        -- ">>"
    | BitLShift        -- "<<"
    | BitOr            -- "|"
    | BitXor           -- "^"
    | BitXnor          -- "!^"
    | BitAnd           -- "&"
    | BitReverse       -- "!"

    -- Arithmetic operators
    | Plus             -- "+"
    | Minus            -- "-"
    | Multiply         -- "*"
    | Divide           -- "/"
    | Modulo           -- "%"
    | Power            -- "**"

    -- Increment / decrement
    | PlusPlus         -- "++"
    | MinusMinus       -- "--"

    -- Other symbols
    | At               -- "@"
    | Dollar           -- "$"
    | LParen           -- "("
    | RParen           -- ")"
    | LBracket         -- "["
    | RBracket         -- "]"
    | LBrace           -- "{"
    | RBrace           -- "}"

    -- Functional / miscellaneous symbols
    | DoubleQuote      -- "\""
    | SingleQuote      -- "'"
    | Semicolon        -- ";"
    | Comma            -- ","
    | Question         -- "?"
    | Colon            -- ":"
    | QuestionArrow    -- "?->"
    | Dot              -- "."
    | DoubleDot        -- ".."
    | Backslash        -- "\\"

    -- Comments / preprocessor
    | LineComment      -- "//"
    | Hash             -- "#"
    | BlockCommentStart -- "/*"
    | BlockCommentEnd   -- "*/"
    deriving (Eq, Show, Ord)


-- | The 'Token' data type represents all possible tokens that can appear
-- in the source code after lexical analysis.
-- Each token carries a position from the source code for error reporting.
data Token = Error String Position
    | CharConst Char Position -- contain ''
    | StrConst String Position -- contain ""
    | NumberConst String Position

    | Ident String Position
    | Symbol Symbol Position
    deriving (Eq)


-- | 'Show' instance for 'Token' provides a human-readable representation
-- of each token, including its value (if applicable) and its source position.
instance Show Token where
    show (Error e pos) = "Error at: " ++ show pos ++ e
    show (CharConst c pos) = "Char@'" ++ [c] ++ "' " ++ show pos
    show (StrConst s pos) = "String@\"" ++ s ++ "\"  " ++ show pos
    show (NumberConst s pos) = "Number@" ++ s ++ " " ++ show pos
    show (Ident name pos) = "Ident@" ++ name ++ " " ++ show pos
    show (Symbol sym pos) = "Symbol@" ++ show sym ++ " " ++ show pos


isErrToken :: Token -> Bool
isErrToken (Error _ _) = True
isErrToken _ = False