module Util.Basic where

import Text.Regex.TDFA ((=~), getAllMatches)

import qualified Data.Char as DC
import qualified Data.List as DL

import qualified Util.Types as DT

-- | Check if a character is valid in an identifier (alphanumeric or underscore).
isIdentChar :: Char -> Bool
isIdentChar c = DC.isAlphaNum c || c == '_'

-- | integer pattern and long pattern
intPattern, longPattern :: String
intPattern = "[+-]?[0-9]+"
longPattern = intPattern ++ "(l|L)"

-- | int pattern and long pattern in hex
hexIntPattern, hexLongPattern :: String
hexIntPattern = "[+-]?0(x|X)[0-9a-fA-F]+"
hexLongPattern = hexIntPattern ++ "(l|L)"

-- | float pattern and double pattern
floatPattern, doublePattern, longDoublePattern :: String
floatPattern = doublePattern ++ "(f|F)"
doublePattern = "[+-]?((([0-9]+([.][0-9]*)|[.][0-9]+)([eE][+-]?[0-9]+)?)|([0-9]+([eE][+-]?[0-9]+)))"
longDoublePattern = doublePattern ++ "(l|L)"

-- | it the regular expression is full matched
full :: DT.Regex -> DT.Regex
full x = "^(" ++ x ++ ")$"

-- | check a string is int, long, hexInt, hexLong, float, double or longDouble
isInt, isLong, isHexInt, isHexLong, isFloat, isDouble, isLongDouble :: String -> Bool
isInt s = s =~ full intPattern
isLong s = s =~ full longPattern
isHexInt s = s =~ full hexIntPattern
isHexLong s = s =~ full hexLongPattern
isFloat s = s =~ full floatPattern
isDouble s = s =~ full doublePattern
isLongDouble s = s =~ full longDoublePattern

-- | check a string is integer, rational or number
integerPattern, rationalPattern, numberPattern :: String
integerPattern = DL.intercalate "|" [intPattern, hexIntPattern, longPattern,hexLongPattern]
rationalPattern = DL.intercalate "|" [floatPattern, doublePattern, longDoublePattern]
numberPattern = DL.intercalate "|" [rationalPattern, integerPattern]

-- | check a string is integer, rational or number
isInteger, isRational, isNumber :: String -> Bool
isInteger s = s =~ full integerPattern
isRational s = s =~ full rationalPattern
isNumber s = s =~ full numberPattern

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