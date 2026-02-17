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
--   * 'positions' — the line/column position of the error
--   * 'index'         — the absolute character index in the source
--
-- It is designed to be embedded inside higher-level error types.
data BasicError = BasicError {

    -- | Path of the source file in which the error occurred.
    filePath :: Path,

    -- | Line and column position of the error.
    positions :: [Position],
 
    -- | What the hell is going on?
    why :: String
} deriving (Eq, Show, Generic)


-- | Construct a 'BasicError' given the file path, position, and reason.
makeError :: Path -> [Position] -> String -> BasicError
makeError path pos reason = BasicError {filePath = path, positions = pos, why = reason}


-- | General internal error message
internalErrorMsg :: String
internalErrorMsg = "internal error"

-- | Internal error: class scope exists but class type stack is empty
missingClassTypeMsg :: String
missingClassTypeMsg = "class scope exists but class type stack is empty"


-- | this error refer to some sytax is not supported in current version, but it will support in latter version.
unsupportedErrorMsg :: String
unsupportedErrorMsg = "syntax unsopported"


-- | Error message for an undefined
undefinedIdentity :: String -> String
undefinedIdentity name = concat ["'", name, "' is not defined in this context."]


-- | Error message for unsupported function name
invalidFunctionName :: String 
invalidFunctionName = "invalid function name"

-- | Error message for ambiguous overload resolution
ambiguousCallMsg :: String
ambiguousCallMsg = "ambiguous call"


-- | Error message for multiple package declarations
multiplePackageMsg :: String
multiplePackageMsg = "multiple package declarations" 


-- | Error message for multiple import statements
multipleVariableDefMsg :: String -> String
multipleVariableDefMsg varName = "multiple definitions of variable: " ++ varName


-- | Error message for multiple function definitions
multipleFunctionDefMsg :: String -> String
multipleFunctionDefMsg funcName = "multiple definitions of function: " ++ funcName

-- | Error message for duplicate method signature.
duplicateMethodMsg :: String -> String
duplicateMethodMsg sig = "duplicate method: " ++ sig


-- | Error message for an invalid string literal
unclosedStrLiteralMsg :: String
unclosedStrLiteralMsg = "unterminated string literal"


-- | Error message for an invalid char literal
unclosedCharLiteralMsg :: String
unclosedCharLiteralMsg = "unterminated character literal"


-- | invalid number formate, usually a number started identity
invalidNumericLiteralMsg :: String
invalidNumericLiteralMsg = "invalid numeric literal"


-- | Error message for an invalid character literal
invalidCharLiteralMsg :: String
invalidCharLiteralMsg = "invalid character literal"


-- | Error message for a comment that was not properly closed
unclosedCommentMsg :: String
unclosedCommentMsg = "unterminated comment"


-- | Error message for a mismatched bracket
mismatchedBracketMsg :: String
mismatchedBracketMsg = "mismatched bracket"


-- | assign error like this: 10 = a;
assignErrorMsg :: String
assignErrorMsg = "the left-hand side of an assignment must be a variable"

-- | for-init must be an assignment (or empty)
forInitAssignMsg :: String
forInitAssignMsg = "for-init must be an assignment"


-- | cannot assign for non-lvalue left-hand side
cannotAssignMsg :: String -> String
cannotAssignMsg s = concat ["\"", s, "\" cannot be assigned"]


-- | implicit cast warning message
implicitCastMsg :: String -> String -> String
implicitCastMsg fromT toT = concat ["implicit cast: ", fromT, " to ", toT]


-- | type mismatch error message
typeMismatchMsg :: String -> String -> String
typeMismatchMsg expected actual = concat["type mismatch: expected ", expected, ", got ", actual]


-- | possible overflow warning message for casts
overflowCastMsg :: String -> String -> String
overflowCastMsg fromT toT = concat ["possible overflow: ", fromT, " -> ", toT]


-- |  Expected expression
expectedExpression :: Int -> String -> String
expectedExpression pos s
    | pos == 0 = concat ["expected an expression before: '", s, "'"]
    | otherwise = concat ["expected an expression after: '", s, "'"]


-- | invalid position for continue control flow
continueCtrlErrorMsg :: String
continueCtrlErrorMsg = "`continue` can only be used inside a loop"


-- | invalid position for break control flow
breakCtrlErrorMsg :: String
breakCtrlErrorMsg = "`break` can only be used inside a loop or a case"


-- | invalid position for return control flow
returnCtrlErrorMsg :: String
returnCtrlErrorMsg = "`return` can only be used inside a function"


-- | Illegal statement means that this one cannot 
illegalStatementMsg :: String -> String -> String
illegalStatementMsg child parrent = concat ["Illegal statement ", child, " in ", parrent," body"]


-- | error message for illegal cast between basic and class types
staticCastError :: String -> String -> String
staticCastError fromT toT = concat ["cannot cast between basic type and class type: ", fromT, " to ", toT]


-- | Basic warning information shared by lexer and parser warnings.
--
-- This structure records:
--
--   * 'filePath'       — the source file where the warning occurred
--   * 'positions' — the line/column position of the warning
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
    | ImplicitCast BasicWarning
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
getErrorMessage (Lexer be) = toJSON be
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
