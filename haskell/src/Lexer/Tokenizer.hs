module Lexer.Tokenizer where

import Text.Regex.TDFA ((=~))

import Control.Monad.State.Strict (State, get, put, modify)
import qualified Data.List as DL
import qualified Data.Ord as DO
import qualified Data.Char as DC


import qualified Util.Exception as UE
import qualified Util.Types as UTypes
import qualified Util.Basic as UB
import qualified Util.Trie as UT


{-- 
 | all symbol in X language
    =, ==, !=, >, <    
    >=, <=, <<, <<=
    >>, >>=, |, |=
    ^, ^=, !^, !^=
    &, $, +, +=
    -, -=, *, *=    
    /, /=, %, %=      
    **, **=, !
    ++, --, @, $
    (, ), [, ], {, }

    ?, : , ?->, ,,, ;
--}
symbols :: [(String, Int)]
symbols = [
    ("=", 0), ("<<=", 0), (">>=", 0), ("|=", 0), ("^=", 0),
    ("!^=", 0), ("+=", 0), ("-=", 0), ("*=", 0), ("/=", 0),
    ("%=", 0), ("**=", 0),

    ("==", 1), ("!=", 1),
    (">", 2), ("<", 2), (">=", 2), ("<=", 2),
    (">>", 3), ("<<", 3),
    ("|", 4),
    ("^", 5), ("!^", 5),
    ("&", 6),
    ("+", -1), ("-", -1),
    ("*", 8), ("/", 8), ("%", 8),
    ("**", 9),
    ("!", 10),
    ("++", -1), ("--", -1),
    ("@", 13), ("$", 13),
    ("(", 14), (")", 14), ("[", 14), ("]", 14), ("{", 14),  ("}", 14),

    -- functional symbols
    ("\"", -2), ("'", -2), (";", -2),
    ("?", -2), (":", -2), ("?->", -2), ("..", -2), ("\\", -2),
    ("//", -3), ("#", -3), ("/*", -3), ("*/", -3)]

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
    | Dollar           -- "%"  -- 注意这里 "%" 出现在 symbols 表里两次，你可能需要确认用途
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
    | Question         -- "?"
    | Colon            -- ":"
    | QuestionArrow    -- "?->"
    | DoubleDot        -- ".."
    | Backslash        -- "\\"

    -- Comments / preprocessor
    | LineComment      -- "//"
    | Hash             -- "#"
    | BlockCommentStart -- "/*"
    | BlockCommentEnd   -- "*/"
    deriving (Eq, Show, Ord)

data Token = End
           | CharConst Char UTypes.Position
           | StrConst String UTypes.Position -- exclude ""
           | IntConst String UTypes.Position
           | LongConst String UTypes.Position

           | FloatConst String UTypes.Position
           | DoubleConst String UTypes.Position
           | Float128Const String UTypes.Position

           | Dot UTypes.Position
           | Ident String UTypes.Position
           | Symbol Symbol UTypes.Position

symbolsTree :: UT.TrieTree Char
symbolsTree = UT.build $ map fst symbols

matchBracket :: Char -> Maybe Char
matchBracket '(' = Just ')'
matchBracket '[' = Just ']'
matchBracket '{' = Just '}'
matchBracket _  = Nothing

eatSymbol :: String -> (String, String)
eatSymbol = UT.eatLongest symbolsTree

eatIdentity :: String -> (String, String)
eatIdentity = span UB.isIdentChar

data CommentState = Normal | InString | InChar | InBlockComment

eatBlockComments :: String -> String
eatBlockComments = go Normal
  where
    go _ [] = []
    go Normal ('"':cs) = '"' : go InString cs
    go Normal ('\'':cs) = '\'' : go InChar cs
    go Normal ('/':'*':cs) = ' ' : ' ' : go InBlockComment cs
    go Normal (c:cs) = c : go Normal cs

    go InString ('\\':c:cs) = '\\' : c : go InString cs
    go InString ('"':cs) = '"' : go Normal cs
    go InString (c:cs) = c : go InString cs

    go InChar ('\\':c:cs) = '\\' : c : go InChar cs
    go InChar ('\'':cs) = '\'' : go Normal cs
    go InChar (c:cs) = c : go InChar cs

    go InBlockComment ('*':'/':cs) = ' ' : ' ' : go Normal cs
    go InBlockComment ('\n':cs) = '\n' : go InBlockComment cs
    go InBlockComment (_:cs) = ' ' : go InBlockComment cs


eatSpace :: String -> (Int, String)
eatSpace [] = (0, [])
eatSpace str@(x:xs)
    | x == '\t' = let (n, rest) = eatSpace xs in (n + 4, rest)
    | DC.isSpace x = let (n, rest) = eatSpace xs in (n + 1, rest)
    | otherwise = (0, str)


eatByPattern :: UTypes.Regex -> String -> (String, String)
eatByPattern pat s = let (_, matched, rest) = s =~ pat :: (String, String, String) in (matched, rest)

eatNumber :: String -> (String, String)
eatNumber s = case eatByPattern (UB.start UB.numberPattern) s of
    ("", _) -> ("", s)
    result -> result

-- | Eat string literal in code
-- | first must be ", otherwise Nothing will be returned
eatStringLiteral :: String -> Maybe (String, String)
eatStringLiteral s =
    let (matched, rest) = eatByPattern (UB.start UB.stringLiteralPattern) s
    in if null matched
        then Nothing
        else Just (matched, rest)

eatCharLiteral :: String -> Maybe (String, String)
eatCharLiteral s =
    let (matched, rest) = eatByPattern (UB.start UB.charLiteralPattern) s
    in if null matched
        then Nothing
        else Just (matched, rest)


data TokenizerState = TokenizerState {
    position :: UTypes.Position,
    code :: [String]}

makeState :: UTypes.Position -> [String] -> TokenizerState
makeState p ls = TokenizerState {position = p, code = ls}
  

eat :: State TokenizerState (Either UE.Error Token)
eat = do
    state <- get
    let (ln, col, _) = UTypes.positionToTuple (position state)
    let strs = code state
    
    if null strs then return $ Right End
    else let
        line = head strs
        rest = tail strs
        in case line of
            [] -> do
                put $ makeState (UTypes.makePosition (succ ln) 0 0) rest
                eat
            (c:_) 
                | UB.isIdentChar c -> return $ Right End
                | otherwise -> return $ Right End