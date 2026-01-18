module Util.Basic where

import Text.Regex.TDFA (Regex, makeRegex)
import Text.Regex.Base.RegexLike (matchTest)


intLitPat, longLitPat :: String

-- | Regular expression for a decimal integer literal.
--
-- Supports:
--   * optional leading '+' or '-'
--   * one or more decimal digits
--
-- Examples:
--   123, -42, +0
intLitPat = "[+-]?[0-9]+"

-- | Regular expression for a decimal long integer literal.
--
-- Same as 'intLitPat', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   123L, -42l
longLitPat = intLitPat ++ "(l|L)"


hexIntLitlPat, hexLongLitPat :: String

-- | Regular expression for a hexadecimal integer literal.
--
-- Supports:
--   * optional leading '+' or '-'
--   * '0x' or '0X' prefix
--   * one or more hexadecimal digits
--
-- Examples:
--   0xFF, -0X10
hexIntLitlPat = "[+-]?0(x|X)[0-9a-fA-F]+"

-- | Regular expression for a hexadecimal long integer literal.
--
-- Same as 'hexIntLitlPat', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   0xFFL, -0x10l
hexLongLitPat = hexIntLitlPat ++ "(l|L)"


floatLitPat, doubleLitPat, longDoubleLitPat :: String
-- | Regular expression for a float literal.
--
-- A float literal is a double literal followed by 'f' or 'F'.
--
-- Examples:
--   1.0f, -3.14F, 1e10f
floatLitPat = doubleLitPat ++ "(f|F)"

-- | Regular expression for a double literal.
--
-- Supports:
--   * decimal form with optional fractional part
--   * scientific notation using 'e' or 'E'
--   * optional leading '+' or '-'
--
-- Examples:
--   1.0, .5, 3., 1e10, -2.3E-4
doubleLitPat = "[+-]?((([0-9]+([.][0-9]*)|[.][0-9]+)([eE][+-]?[0-9]+)?)|([0-9]+([eE][+-]?[0-9]+)))"

-- | Regular expression for a long double literal.
--
-- Same as 'doubleLiteralPattern', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   1.0L, 3e5l
longDoubleLitPat = doubleLitPat ++ "(l|L)"


trueLitPat, falseLitPat, boolLitPat :: String

-- | Regular expression for true literals.
--
-- Accepts both lowercase and capitalized variants.
--
-- Examples:
--   true and True
trueLitPat = "true|True"

-- | Regular expression for false literals.
--
-- Accepts both lowercase and capitalized variants.
--
-- Examples:
--   true and True
falseLitPat = "false|False"


-- | Regular expression for false literals.
--
-- Accepts both lowercase and capitalized variants.
--
-- Examples:
--   false, False, true and True
boolLitPat = trueLitPat ++ ('|' : falseLitPat)



intLitReg, longLitReg, hexIntLitlReg, hexLongLitReg, floatLitReg, doubleLitReg, longDoubleLitReg :: Regex
intLitReg = makeRegex $ full intLitPat
longLitReg = makeRegex $ full longLitPat
hexIntLitlReg = makeRegex $ full hexIntLitlPat
hexLongLitReg = makeRegex $ full hexLongLitPat
floatLitReg = makeRegex $ full floatLitPat
doubleLitReg = makeRegex $ full doubleLitPat
longDoubleLitReg = makeRegex $ full longDoubleLitPat


intReg, longReg :: Regex
intReg = makeRegex $ full $ intLitPat ++ ('|' : hexIntLitlPat)
longReg = makeRegex $ full $ longLitPat ++ ('|' : hexLongLitPat)


trueLitReg, falseLitReg, boolLitReg :: Regex
trueLitReg = makeRegex $ full trueLitPat
falseLitReg = makeRegex $ full falseLitPat
boolLitReg = makeRegex $ full boolLitPat



-- | Anchor a regular expression to match the entire input.
--
-- Equivalent to wrapping the pattern with '^(' and ')$'.
--
-- Used to ensure a string is fully matched by a pattern.
full :: String -> String
full s = "^(" ++ s ++ ")$"


-- | Check whether a regex matches a string (partial match allowed).
--
-- This uses the TDFA '=~' operator directly.
match :: Regex -> String -> Bool
match = matchTest


-- | Check whether a string is any kind of integer literal.
isInt :: String -> Bool
isInt = match intReg
    

-- | Check whether a string is a long double literal.
isLong :: String -> Bool
isLong = match longReg


-- | Check whether a string is a float literal.
isFloat :: String -> Bool
isFloat = match floatLitReg


-- | Check whether a string is a double literal.
isDouble :: String -> Bool
isDouble = match doubleLitReg


-- | Check whether a string is a long double literal.
isLongDouble :: String -> Bool
isLongDouble = match longDoubleLitReg


-- | Check whether a string is a boolean true literal.
isBoolTrue :: String -> Bool
isBoolTrue = match trueLitReg


-- | Check whether a string is a boolean true literal.
isBoolFalse :: String -> Bool
isBoolFalse = match falseLitReg


-- | Check whether a string is a boolean literal.
isBool :: String -> Bool
isBool = match boolLitReg
