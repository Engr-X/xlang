module Util.FileHelper where

import Util.Types
import qualified Util.Exception as Exception

import qualified Data.Text as DText
import qualified Data.Text.IO as DTIO
import qualified System.IO.Error as SIE

import Control.Exception (IOException)


-- | cast IOException to Error
classifyError :: IOException -> Exception.Error
classifyError = Exception.ReadError . show

processContent :: String -> [(Int, String)]
processContent = filter (\(_, b) -> not (null b)) . zip [1 ..] . lines . filter (/= '\r')

readFile :: Path -> IO (Either Exception.Error String)
readFile path = do
    result <- SIE.tryIOError (DTIO.readFile path)
    case result of
        Left e  -> return $ Left $ classifyError e
        Right contents -> return $ Right $ DText.unpack contents
