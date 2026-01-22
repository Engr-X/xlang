{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Exception where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)
import Util.Type

import qualified Data.Text as DText
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTL


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
 
    -- | What the hell is going on?
    why :: String
} deriving (Eq, Show, Generic)


-- | Construct a 'BasicError' given the file path, position, and reason.
makeError :: Path -> Position -> String -> BasicError
makeError path pos reason = BasicError {filePath = path, startPosition = pos, why = reason}


internalErrorMsg :: String
internalErrorMsg = "internal error"


-- Error message for an invalid string literal
unclosedStrLiteralMsg :: String
unclosedStrLiteralMsg = "unterminated string literal"


-- Error message for an invalid char literal
unclosedCharLiteralMsg :: String
unclosedCharLiteralMsg = "unterminated character literal"


-- invalid number formate, usually a number started identity
invalidNumericLiteralMsg :: String
invalidNumericLiteralMsg = "invalid numeric literal"


-- Error message for an invalid character literal
invalidCharLiteralMsg :: String
invalidCharLiteralMsg = "invalid character literal"


-- Error message for a comment that was not properly closed
unclosedCommentMsg :: String
unclosedCommentMsg = "unterminated comment"


-- Error message for a mismatched bracket
mismatchedBracket :: String
mismatchedBracket = "mismatched bracket"


-- Expected expression
expectedExpression :: Int -> String -> String
expectedExpression pos s
    | pos == 0 = "expected an expression before: '" ++ s ++ "'"
    | otherwise = "expected an expression after: '" ++ s ++ "'"
    


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
--   * 'Reading'  — file I/O failure
--   * 'Syntax' — syntax parsing error
--   * 'Lex' — lexical analysis error
--   * 'Parsing'— syntax parsing error
--
-- Each error variant carries the minimum information required for reporting.
data ErrorKind = None
    | Unkown Path
    | Reading Path
    | Lexer BasicError 
    | Parsing BasicError
    | Syntax BasicError
    deriving (Eq, Show)


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


-- | Convert an 'ErrorKind' to a numeric error code.
--
-- These codes are intended for external interfaces such as:
--   * command-line exit codes
--   * JSON diagnostics
--   * test assertions
--
-- ErrorKind code mapping:
--   * 0 — No error
--   * 1 — File read error
--   * 2 — Lex error
--   * 3 — Parser error
getErrorCode :: ErrorKind -> Int
getErrorCode (Unkown _) = -1
getErrorCode None = 0
getErrorCode (Reading _)  = 1
getErrorCode (Lexer _) = 2
getErrorCode (Syntax _) = 3
getErrorCode (Parsing _) = 4


-- | Convert an 'ErrorKind' into a JSON value representing the error message.
--
-- For simple errors, this returns a human-readable string.
-- For structured errors (lexer / parser), this returns a JSON object
-- derived from 'BasicError'.
--
-- This separation allows consumers to distinguish error categories
-- programmatically.
getErrorMessage :: ErrorKind -> Value
getErrorMessage (Unkown path) = String ("Unknown error occur at: " <> DText.pack path)
getErrorMessage None = String ""
getErrorMessage (Reading path) = String $ "Cannot open file: " <> DText.pack path
getErrorMessage (Lexer be)  = toJSON be
getErrorMessage (Syntax be) = toJSON be
getErrorMessage (Parsing be) = toJSON be


-- | Convert an 'ErrorKind' into a pretty-printed JSON string.
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
errorToString :: ErrorKind -> String
errorToString err = DText.unpack $ DTL.toStrict $ DTL.decodeUtf8 $ encodePretty $ object [
    "code"  .= getErrorCode err, 
    "error" .= getErrorMessage err]
