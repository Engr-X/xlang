module Util.Basic where

import Text.Regex.TDFA ((=~), getAllMatches)

import qualified Data.Char as DC
import qualified Data.List as DL

import qualified Util.Types as DT


-- | Check whether a character can appear in an identifier.
--
-- Valid identifier characters include:
--   * alphabetic characters (A–Z, a–z)
--   * digits (0–9)
--   * underscore '_'
--
-- This is typically used when scanning identifiers in a lexer.
isIdentChar :: Char -> Bool
isIdentChar c = DC.isAlphaNum c || c == '_'


intLiteralPattern, longLiteralPattern :: String

-- | Regular expression for a decimal integer literal.
--
-- Supports:
--   * optional leading '+' or '-'
--   * one or more decimal digits
--
-- Examples:
--   123, -42, +0
intLiteralPattern = "[+-]?[0-9]+"

-- | Regular expression for a decimal long integer literal.
--
-- Same as 'intLiteralPattern', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   123L, -42l
longLiteralPattern = intLiteralPattern ++ "(l|L)"


hexIntLiteralPattern, hexLongLiteralPattern :: String

-- | Regular expression for a hexadecimal integer literal.
--
-- Supports:
--   * optional leading '+' or '-'
--   * '0x' or '0X' prefix
--   * one or more hexadecimal digits
--
-- Examples:
--   0xFF, -0X10
hexIntLiteralPattern = "[+-]?0(x|X)[0-9a-fA-F]+"

-- | Regular expression for a hexadecimal long integer literal.
--
-- Same as 'hexIntLiteralPattern', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   0xFFL, -0x10l
hexLongLiteralPattern = hexIntLiteralPattern ++ "(l|L)"


floatLitreralPattern, doubleLiteralPattern, longDoubleLiteralPattern :: String
-- | Regular expression for a float literal.
--
-- A float literal is a double literal followed by 'f' or 'F'.
--
-- Examples:
--   1.0f, -3.14F, 1e10f
floatLitreralPattern = doubleLiteralPattern ++ "(f|F)"

-- | Regular expression for a double literal.
--
-- Supports:
--   * decimal form with optional fractional part
--   * scientific notation using 'e' or 'E'
--   * optional leading '+' or '-'
--
-- Examples:
--   1.0, .5, 3., 1e10, -2.3E-4
doubleLiteralPattern = "[+-]?((([0-9]+([.][0-9]*)|[.][0-9]+)([eE][+-]?[0-9]+)?)|([0-9]+([eE][+-]?[0-9]+)))"

-- | Regular expression for a long double literal.
--
-- Same as 'doubleLiteralPattern', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   1.0L, 3e5l
longDoubleLiteralPattern = doubleLiteralPattern ++ "(l|L)"


-- | Regular expression for a string literal.
--
-- Supports:
--   * double-quoted strings
--   * escaped characters using backslash (e.g. \" \\ \n)
--
-- Does NOT allow unescaped double quotes inside the string.
--
-- Examples:
--   "hello", "a\\nb", "\"quoted\""
stringLiteralPattern :: String
stringLiteralPattern = "\"([^\"\\\\]|\\\\.)*\""


-- | Regular expression for a character literal.
--
-- Supports:
--   * single character literals: 'a'
--   * escaped characters: '\\n', '\\t', '\\''
--   * hexadecimal escapes: '\\xFF'
--   * octal escapes: '\\123'
--
-- Examples:
--   'a', '\\n', '\\x41'
charLiteralPattern :: String
charLiteralPattern = "'(\\\\(x[0-9a-fA-F]+|[0-7]{1,3}|[nrt\"'\\\\abfv0])|[^'\\\\])'"


-- | Regular expression for a multi-line (block) comment.
--
-- Matches comments of the form:
--   /* ... */
--
-- This pattern ensures that the closing '*/' is properly matched.
multlineCommentPattern :: String
multlineCommentPattern = "/\\*([^*]|\\*[^/])*\\*/" 


-- | Regular expression for boolean literals.
--
-- Accepts both lowercase and capitalized variants.
--
-- Examples:
--   true, false, True, False
boolLiteralPattern :: String
boolLiteralPattern = "true|false|True|False"


-- | Anchor a regular expression to the start of the input.
--
-- Equivalent to prepending '^(' and appending ')'.
--
-- Useful when matching prefixes during lexical analysis.
start :: DT.Regex -> DT.Regex
start x = "^(" ++ x ++ ")"


-- | Anchor a regular expression to match the entire input.
--
-- Equivalent to wrapping the pattern with '^(' and ')$'.
--
-- Used to ensure a string is fully matched by a pattern.
full :: DT.Regex -> DT.Regex
full x = "^(" ++ x ++ ")$"


isIntLiteral, isLongLiteral, isHexIntLiteral, isHexLongLiteral :: String -> Bool
-- | Check whether a string is a decimal integer literal.
isIntLiteral = matchFull intLiteralPattern

-- | Check whether a string is a long integer literal.
isLongLiteral = matchFull longLiteralPattern

-- | Check whether a string is a hexadecimal int integer literal.
isHexIntLiteral = matchFull hexIntLiteralPattern

-- | Check whether a string is a hexadecimal long integer literal.
isHexLongLiteral = matchFull hexLongLiteralPattern


isFloatLiteral, isDoubleLiteral, isLongDoubleLiteral :: String -> Bool
-- | Check whether a string is a float literal.
isFloatLiteral = matchFull floatLitreralPattern

-- | Check whether a string is a double literal.
isDoubleLiteral = matchFull doubleLiteralPattern

-- | Check whether a string is a long double literal.
isLongDoubleLiteral = matchFull longDoubleLiteralPattern


-- | Check whether a string is a string literal.
isStringLiteral :: String -> Bool
isStringLiteral = matchFull stringLiteralPattern


-- | Check whether a string is a character literal.
isCharLiteral :: String -> Bool
isCharLiteral = matchFull charLiteralPattern


-- | Check whether a string is a boolean literal.
isBoolLiteral :: String -> Bool
isBoolLiteral = matchFull boolLiteralPattern


integerPattern, rationalPattern, numberPattern :: String
-- | Regular expression for all integer literals (decimal and hexadecimal).
integerPattern = DL.intercalate "|" [intLiteralPattern, hexIntLiteralPattern, longLiteralPattern,hexLongLiteralPattern]

-- | Regular expression for all rational (floating-point) literals.
rationalPattern = DL.intercalate "|" [floatLitreralPattern, doubleLiteralPattern, longDoubleLiteralPattern]

-- | Regular expression for any numeric literal (integer or rational).
numberPattern = DL.intercalate "|" [rationalPattern, integerPattern]


isInteger, isRational, isNumber :: String -> Bool
-- | Check whether a string is any kind of integer literal.
isInteger = matchFull integerPattern

-- | Check whether a string is any kind of rational (floating-point) literal.
isRational = matchFull rationalPattern

-- | Check whether a string is any numeric literal.
isNumber = matchFull numberPattern

--------------------------------------------------------------------------------
-- Regex matching utilities
--------------------------------------------------------------------------------


-- | Check whether a regex matches a string (partial match allowed).
--
-- This uses the TDFA '=~' operator directly.
match :: DT.Regex -> String -> Bool
match = flip (=~)


-- | Check whether a regex matches the start of a string.
--
-- This uses the TDFA '=~' operator directly.
matchStart :: DT.Regex -> String -> Bool
matchStart reg = match (start reg)


-- | Check whether a regex fully matches a string.
--
-- Internally anchors the pattern using 'full'.
matchFull :: DT.Regex -> String -> Bool
matchFull reg = match (full reg)


-- | Compute the length of the prefix matched by a regex.
--
-- The pattern is anchored at the start of the string.
-- Returns 0 if no prefix match exists.
--
-- Useful when consuming tokens in a lexer.
matchLength :: DT.Regex -> String -> Int
matchLength pat s = let (_, matched, _) = s =~ ("^(" ++ pat ++ ")") :: (String, String, String) in length matched


-- | Find all non-overlapping matches of a regex pattern in a string.
--
-- Returns a list of triples:
--   * matched substring
--   * starting offset
--   * length of the match
--
-- Matches of zero length are ignored.
--
-- Useful for locating tokens, comments, or literals inside source code.
findMatches :: DT.Regex -> String -> [(String, Int, Int)]
findMatches pat s
  | null pat = []
  | otherwise = [
      (take len (drop off s), off, len)
      | (off, len) <- getAllMatches (s =~ pat) :: [(Int, Int)]
      , len > 0]