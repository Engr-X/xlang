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


data BasicError = BasicError {
    filePath :: Path,
    startPosition :: Position,
    index :: Int
} deriving (Show, Generic)

instance ToJSON BasicError

data Error = None
           | ReadError Path
           | LexerError BasicError 
           | ParingError BasicError
           deriving (Show)

-- | error code for exceptions
getErrorCode :: Error -> Int
getErrorCode None           = 0
getErrorCode (ReadError _)  = 1
getErrorCode (LexerError _) = 2
getErrorCode (ParingError _) = 3


getErrorMessage :: Error -> Value
getErrorMessage None           = String ""
getErrorMessage (ReadError path) = String $ "Cannot open file: " <> DText.pack path
getErrorMessage (LexerError be)  = toJSON be
getErrorMessage (ParingError be) = toJSON be


errorToString :: Error -> String
errorToString err = DText.unpack $ DTL.toStrict $ DTL.decodeUtf8 $ encodePretty $ object [
    "code"  .= getErrorCode err, 
    "error" .= getErrorMessage err]
