module Lexer.Tokenizer where

import Text.Regex.TDFA ((=~), getAllMatches)

import qualified Data.Char as DC
import qualified Data.List as DT

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
    ("=", 0), ("<<=", 0), (">>=", 0), ("|=", 0), ("^=", 0), ("!^=", 0), ("+=", 0), ("-=", 0), ("*=", 0), ("/=", 0), ("%=", 0), ("**=", 0),
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
    ("@", 13), ("%", 13),
    ("(", 14), (")", 14), ("[", 14), ("]", 14), ("{", 14),  ("}", 14),

    -- functional symbols
    ("\"", -2), ("'", -2), (";", -2),
    ("?", -2), (":", -2), ("?->", -2), ("..", -2), ("\\", -2),
    ("//", -2)]


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


eatByPattern :: UTypes.Regex -> String -> (String, String)
eatByPattern pat s = let (_, matched, rest) = s =~ pat :: (String, String, String) in (matched, rest)

eatNumber :: String -> (String, String)
eatNumber s = case eatByPattern (UB.full UB.numberPattern) s of
    ("", _) -> ("", s)
    result -> result

-- | Eat string literal in code
-- | first must be ", otherwise Nothing will be returned
eatStringLiteral :: String -> Maybe (String, String)
eatStringLiteral s =
    let (matched, rest) = eatByPattern UB.stringLiteralPattern s
    in if null matched
        then Nothing
        else Just (matched, rest)

eatCharLiteral :: String -> Maybe (String, String)
eatCharLiteral s =
    let (matched, rest) = eatByPattern UB.charLiteralPattern s
    in if null matched
        then Nothing
        else Just (matched, rest)
