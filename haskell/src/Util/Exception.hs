{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Exception where

import qualified Data.Text as DText
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTL
import GHC.Generics (Generic)

import Util.Types


-- | Basic error information shared by lexer and parser errors.
--
-- This structure records:
--
--   * 'filePath'       — the source file where the error occurred
--   * 'startPosition' — the line/column position of the error
--   * 'index'         — the absolute character index in the source
--
-- It is designed to be embedded inside higher-level error types.
data BasicError = BasicError {

    -- | Path of the source file in which the error occurred.
    filePath :: Path,

    -- | Line and column position of the error.
    startPosition :: Position,

    -- | Absolute index of the error in the input stream.
    index :: Int
} deriving (Show, Generic)


-- | Basic warning information shared by lexer and parser warnings.
--
-- This structure records:
--
--   * 'filePath'       — the source file where the warning occurred
--   * 'startPosition' — the line/column position of the warning
--   * 'index'         — the absolute character index in the source
--
-- It is designed to be embedded inside higher-level error types.
type BasicWarning = BasicError

instance ToJSON BasicError


-- | Unified error type for the compiler.
--
-- This type represents all possible error categories that may occur during
-- compilation, including:
--
--   * 'None'        — no error
--   * 'ReadError'  — file I/O failure
--   * 'LexerError' — lexical analysis error
--   * 'ParingError'— syntax parsing error
--
-- Each error variant carries the minimum information required for reporting.
data Error = None
           | ReadError Path
           | LexerError BasicError 
           | ParingError BasicError
           deriving (Show)

-- | Unified warning type for the compiler.
--
-- This type represents all possible warning categories that may occur during
-- compilation, including:
--
--  * 'Null'               — no warning
--  * 'OverflowWarning'    — numeric overflow warning
--  * 'UnderflowWarning'   — numeric underflow warning
--
-- Each warning variant carries the minimum information required for reporting.
data Warning = Null
             | OverflowWarning BasicWarning
             | UnderflowWarning BasicWarning
             deriving (Show)

-- | Convert an 'Error' to a numeric error code.
--
-- These codes are intended for external interfaces such as:
--   * command-line exit codes
--   * JSON diagnostics
--   * test assertions
--
-- Error code mapping:
--   * 0 — No error
--   * 1 — File read error
--   * 2 — Lexer error
--   * 3 — Parser error
getErrorCode :: Error -> Int
getErrorCode None           = 0
getErrorCode (ReadError _)  = 1
getErrorCode (LexerError _) = 2
getErrorCode (ParingError _) = 3


-- | Convert an 'Error' into a JSON value representing the error message.
--
-- For simple errors, this returns a human-readable string.
-- For structured errors (lexer / parser), this returns a JSON object
-- derived from 'BasicError'.
--
-- This separation allows consumers to distinguish error categories
-- programmatically.
getErrorMessage :: Error -> Value
getErrorMessage None           = String ""
getErrorMessage (ReadError path) = String $ "Cannot open file: " <> DText.pack path
getErrorMessage (LexerError be)  = toJSON be
getErrorMessage (ParingError be) = toJSON be


-- | Convert an 'Error' into a pretty-printed JSON string.
--
-- The resulting JSON object has the following structure:
--
-- @
-- {
--   "code":  <error code>,
--   "error": <error message or object>
-- }
-- @
--
-- This function is typically used as the final step before reporting
-- an error to the user or writing diagnostics to output.
errorToString :: Error -> String
errorToString err = DText.unpack $ DTL.toStrict $ DTL.decodeUtf8 $ encodePretty $ object [
    "code"  .= getErrorCode err, 
    "error" .= getErrorMessage err]
