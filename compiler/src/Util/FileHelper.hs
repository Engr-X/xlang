module Util.FileHelper where

import Control.Exception (IOException)
import Util.Type

import qualified Data.Text as DText
import qualified Data.Text.IO as DTIO
import qualified Util.Exception as UE

import qualified System.IO as SIO
import qualified System.IO.Error as SIE


-- | Classify an 'IOException' into the unified 'Exception.ErrorKind' type.
--
-- Currently, all IO exceptions are treated as 'ReadError' with a message.
-- This allows the rest of the compiler or toolchain to handle file errors
-- in a uniform way.
classifyError :: IOException -> UE.ErrorKind
classifyError = UE.Reading . show



-- | Safely read the contents of a file as a 'String'.
--
-- Returns an 'Either Exception.ErrorKind String' to capture possible I/O errors.
--
-- Behavior:
--   * On success: Right contents of the file (as String)
--   * On failure: Left with a 'ReadError' wrapping the exception message
--
-- Uses 'tryIOError' to catch all standard IO exceptions.
readFile :: Path -> IO (Either UE.ErrorKind String)
readFile path = do
    result <- SIE.tryIOError $
        SIO.withFile path SIO.ReadMode $ \h -> do
            SIO.hSetEncoding h SIO.utf8
            DTIO.hGetContents h

    case result of
        Left e        -> return $ Left (classifyError e)
        Right content -> return $ Right (DText.unpack content)
