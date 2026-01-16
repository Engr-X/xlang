module Lex.Token where

import Util.Type


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
    | Semicolon        -- ";"
    | Comma            -- ","
    | Question         -- "?"
    | Colon            -- ":"
    | QuestionArrow    -- "?->"
    | Dot              -- "."
    | DoubleDot        -- ".."
    | Backslash        -- "\\"
    deriving (Eq, Show, Ord)


-- | The 'Token' data type represents all possible tokens that can appear
-- in the source code after lexical analysis.
-- Each token carries a position from the source code for error reporting.
data Token = Error String Position
    | Ident String Position
    | Symbol Symbol Position
    | NumberConst String Position

    | CharConst Char Position -- contain ''
    | StrConst String Position -- contain ""
    deriving (Eq)


-- | 'Show' instance for 'Token' provides a human-readable representation
-- of each token, including its value (if applicable) and its source position.
instance Show Token where
    show (Ident name pos) = "Ident@" ++ name ++ " " ++ show pos
    show (Symbol sym pos) = "Symbol@" ++ show sym ++ " " ++ show pos
    show (NumberConst s pos) = "Number@" ++ s ++ " " ++ show pos
    show (CharConst c pos) = "Char@'" ++ [c] ++ "' " ++ show pos
    show (StrConst s pos) = "String@\"" ++ s ++ "\"  " ++ show pos
    show (Error e pos) = "Error at: " ++ show pos ++ e


-- | Check whether a token represents a lexer error.
-- Returns True only for 'Error' tokens, False otherwise.
isErrToken :: Token -> Bool
isErrToken (Error _ _) = True
isErrToken _ = False


-- | get the position of the token
tokPos :: Token -> Position
tokPos (Error _ p) = p
tokPos (Ident _ p) = p
tokPos (Symbol _ p) = p
tokPos (NumberConst _ p) = p
tokPos (CharConst _ p) = p
tokPos (StrConst _ p) = p
