{-# LANGUAGE DeriveGeneric #-}
module Util.Type where

import Data.Aeson
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy.Char8 as B


-- | File path represented as a string.
--
-- Used to indicate the location of a source file for errors,
-- lexer/parser input, or I/O operations.
type Path = String


-- | JSON object serialized as a lazy ByteString.
--
-- Useful for structured error reporting, logging, or communication
-- with external tools.
type JSONObject = B.ByteString


-- | Represents a position in a source file.
--
-- Typically used to store the **line** and **column** of a token or error.
--
-- Fields:
--   * 'line'   — line number (1-based)
--   * 'column' — column number (1-based)
--   * 'length' — length of the span in characters
--
-- Example usage:
-- > Position { line = 3, column = 5, length = 2 }
data Position = Position {
    line :: Int,
    column :: Int,
    len :: Int
} deriving (Eq, Generic)

instance Show Position where
    show = show . positionToTuple

instance ToJSON Position


-- | Construct a new 'Position' from its line, column, and token length.
--
-- Position {line = 3, column = 5, len = 2}
makePosition :: Int -> Int -> Int -> Position
makePosition l c size = Position {line = l, column = c, len = size}


-- default position
defaultPosition :: Position
defaultPosition = makePosition (-1) (-1) (-1)


-- | Convert a 'Position' into a tuple (line, column, len).
--
-- Useful for functions that prefer tuple representation.
positionToTuple :: Position -> (Int, Int, Int)
positionToTuple pos = (line pos, column pos, len pos)


-- | A simple range represented as a tuple of start and end indices.
--
-- Usually used to store the span of a substring, token, or text segment
-- in the input stream.
--
-- Example usage:
-- > type Range = (10, 15)  -- represents characters from index 10 to 15 [10, 15)
type Range = (Int, Int)
