{-# LANGUAGE DeriveGeneric #-}

module Lex.Token where

import Util.Type
import GHC.Generics (Generic)
import Data.Hashable (Hashable)


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
    | BitNot           -- "!"

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
    | DoubleColon      -- "::" 
    | Arrow            -- "->"
    | FatArrow         -- "\=>"
    | QuestionArrow    -- "?->"
    | Dot              -- "."
    | DoubleDot        -- ".."
    | Backslash        -- "\\"
    deriving (Eq, Ord, Show, Generic)

instance Hashable Symbol


-- | The 'Token' data type represents all possible tokens that can appear
-- in the source code after lexical analysis.
-- Each token carries a position from the source code for error reporting.
data Token = 
      Error String Position
    | EOF Position
    | TokenPass  Position
    | Ident String Position
    | Symbol Symbol Position
    | NumberConst String Position

    | CharConst Char Position -- contain ''
    | StrConst String Position -- contain ""
    deriving (Eq, Show)


-- | 'Show' instance for 'Token' provides a human-readable representation
-- of each token, including its value (if applicable) and its source position.
prettyToken :: Token -> String
prettyToken (TokenPass  pos) = "nl@" ++ show pos
prettyToken (Ident name pos) = concat ["Ident@", name, " ", show pos]
prettyToken (Symbol sym pos) = concat ["Symbol@", show sym, " ", show pos]
prettyToken (NumberConst s pos) = concat ["Number@", s, " ", show pos]
prettyToken (CharConst c pos) = concat ["Char@'", [c], "' ", show pos]
prettyToken (StrConst s pos) = concat ["String@\"", s, "\"  ", show pos]
prettyToken (Error e pos) = concat ["Error at: ", show pos, e]
prettyToken (EOF pos) = "eof@" ++ show pos


-- | Check whether a token represents a lexer error.
-- Returns True only for 'Error' tokens, False otherwise.
isErrToken :: Token -> Bool
isErrToken (Error _ _) = True
isErrToken _ = False


-- | Check whether a token represents a left bracket.
isLBracketToken :: Token -> Bool 
isLBracketToken (Symbol LParen _) = True
isLBracketToken (Symbol LBracket _) = True
isLBracketToken (Symbol LBrace _) = True
isLBracketToken _ = False


-- | Check whether a token represents a left bracket.
isRBracketToken :: Token -> Bool
isRBracketToken (Symbol RParen _) = True
isRBracketToken (Symbol RBracket _) = True
isRBracketToken (Symbol RBrace _) = True
isRBracketToken _ = False


-- | Check whether a token represents a bracket.
isBracketToken :: Token -> Bool
isBracketToken t = isLBracketToken t || isRBracketToken t 


-- | get the position of the token
tokenPos :: Token -> Position
tokenPos (EOF p) = p
tokenPos (Error _ p) = p
tokenPos (TokenPass  p) = p
tokenPos (Ident _ p) = p
tokenPos (Symbol _ p) = p
tokenPos (NumberConst _ p) = p
tokenPos (CharConst _ p) = p
tokenPos (StrConst _ p) = p
