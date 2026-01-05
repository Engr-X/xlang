module Util.Basic where

import Text.Regex.TDFA ((=~), getAllMatches)

import qualified Data.Char as DC
import qualified Data.List as DL

import qualified Util.Types as DT

-- | Check if a character is valid in an identifier (alphanumeric or underscore).
isIdentChar :: Char -> Bool
isIdentChar c = DC.isAlphaNum c || c == '_'

-- | integer pattern and long pattern
intLiteralPattern, longLiteralPattern :: String
intLiteralPattern = "[+-]?[0-9]+"
longLiteralPattern = intLiteralPattern ++ "(l|L)"

-- | int pattern and long pattern in hex
hexIntLiteralPattern, hexLongLiteralPattern :: String
hexIntLiteralPattern = "[+-]?0(x|X)[0-9a-fA-F]+"
hexLongLiteralPattern = hexIntLiteralPattern ++ "(l|L)"

-- | float pattern and double pattern
floatLitreralPattern, doubleLiteralPattern, longDoubleLiteralPattern :: String
floatLitreralPattern = doubleLiteralPattern ++ "(f|F)"
doubleLiteralPattern = "[+-]?((([0-9]+([.][0-9]*)|[.][0-9]+)([eE][+-]?[0-9]+)?)|([0-9]+([eE][+-]?[0-9]+)))"
longDoubleLiteralPattern = doubleLiteralPattern ++ "(l|L)"

stringLiteralPattern :: String
stringLiteralPattern = "\"([^\"\\\\]|\\\\.)*\""

charLiteralPattern :: String
charLiteralPattern = "'(\\\\[nrt\"'\\\\]|[^'\\\\])'"

boolLiteralPattern :: String
boolLiteralPattern = "true|false|True|False"

-- | it the regular expression is full matched
full :: DT.Regex -> DT.Regex
full x = "^(" ++ x ++ ")$"

-- | check a string is int, long, hexInt, hexLong, float, double or longDouble
isIntLiteral, isLongLiteral, isHexIntLiteral, isHexLongLiteral :: String -> Bool
isIntLiteral = matchFull intLiteralPattern
isLongLiteral = matchFull longLiteralPattern
isHexIntLiteral = matchFull hexIntLiteralPattern
isHexLongLiteral = matchFull hexLongLiteralPattern

isFloatLiteral, isDoubleLiteral, isLongDoubleLiteral :: String -> Bool
isFloatLiteral = matchFull floatLitreralPattern
isDoubleLiteral = matchFull doubleLiteralPattern
isLongDoubleLiteral = matchFull longDoubleLiteralPattern

isStringLiteral :: String -> Bool
isStringLiteral = matchFull stringLiteralPattern

isCharLiteral :: String -> Bool
isCharLiteral = matchFull charLiteralPattern

isBoolLiteral :: String -> Bool
isBoolLiteral = matchFull boolLiteralPattern

-- | check a string is integer, rational or number
integerPattern, rationalPattern, numberPattern :: String
integerPattern = DL.intercalate "|" [intLiteralPattern, hexIntLiteralPattern, longLiteralPattern,hexLongLiteralPattern]
rationalPattern = DL.intercalate "|" [floatLitreralPattern, doubleLiteralPattern, longDoubleLiteralPattern]
numberPattern = DL.intercalate "|" [rationalPattern, integerPattern]

-- | check a string is integer, rational or number
isInteger, isRational, isNumber :: String -> Bool
isInteger = matchFull integerPattern
isRational = matchFull rationalPattern
isNumber = matchFull numberPattern

-- regular expression

-- | Check if a regex matches a string
match :: DT.Regex -> String -> Bool
match = flip (=~)

-- | Check if a regex matches a string
matchFull :: DT.Regex -> String -> Bool
matchFull reg = match (full reg)

-- | Check if a regex matches a string
matchLength :: DT.Regex -> String -> Int
matchLength pat s = let (_, matched, _) = s =~ ("^(" ++ pat ++ ")") :: (String, String, String) in length matched

-- | Find all non-overlapping matches of a regex pattern in a string.
-- Uses `matchesLength` to find the earliest prefix match on each suffix and
-- pattern first (DT.Regex), then subject string
findMatches :: DT.Regex -> String -> [(String, Int, Int)]
findMatches pat s
  | null pat = []
  | otherwise = [
      (take len (drop off s), off, len)
      | (off, len) <- getAllMatches (s =~ pat) :: [(Int, Int)]
      , len > 0]