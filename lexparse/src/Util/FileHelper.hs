module Util.FileHelper where

import Control.Exception (IOException)
import Util.Types

import qualified Data.Text as DText
import qualified Data.Text.IO as DTIO
import qualified Util.Exception as Exception
import qualified System.IO.Error as SIE


-- | Classify an 'IOException' into the unified 'Exception.ErrorKind' type.
--
-- Currently, all IO exceptions are treated as 'ReadError' with a message.
-- This allows the rest of the compiler or toolchain to handle file errors
-- in a uniform way.
classifyError :: IOException -> Exception.ErrorKind
classifyError = Exception.ReadError . show


-- | Split a file content into lines and annotate each line with its line number.
--
-- This function:
--   * Removes carriage return characters ('\r') for cross-platform compatibility
--   * Splits content by newline '\n'
--   * Zips each line with its 1-based line number
--
-- Useful for lexing, syntax highlighting, or line-based diagnostics.
processContent :: String -> [String]
processContent = lines . filter (/= '\r')


-- | Safely read the contents of a file as a 'String'.
--
-- Returns an 'Either Exception.ErrorKind String' to capture possible I/O errors.
--
-- Behavior:
--   * On success: Right contents of the file (as String)
--   * On failure: Left with a 'ReadError' wrapping the exception message
--
-- Uses 'tryIOError' to catch all standard IO exceptions.
readFile :: Path -> IO (Either Exception.ErrorKind String)
readFile path = do
    result <- SIE.tryIOError (DTIO.readFile path)
    case result of
        Left e  -> return $ Left $ classifyError e
        Right contents -> return $ Right $ DText.unpack contents
