{-# LANGUAGE DeriveGeneric #-}

module Util.Types where

import Data.Int (Int32, Int64)

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

-- | path of a file
type Path = String

type Regex = String

type JSONObject = B.ByteString

-- | usually store col and line
data Position = Position {
    line :: Int,
    column :: Int,
    length :: Int
} deriving (Show, Generic)

instance ToJSON Position


-- | eg store range of an string
type Range = (Int, Int)


{-- 
 | all symbol in X language
    =, ==, !=, >, <    
    >=, <=, <<, <<=
    >>, >>=, |, |=
    ^, ^=, !^, !^=
    &, $=, +, +=
    -, -=, *, *=    
    /, /=, %, %=      
    **, **=, !
    ++, --, @, $
    (, ), [, ], {, }

    ?, : , ?->, ,,, ;
--}

data Operator = Asign | Equal | NotEqual | Greater

data Token = CharConst Char Position
           | StrConst String Position
           | IntConst Int32 Position
           | LongConst Int64 Position
           | FloatConst Float Position
           | DoubleConst Double Position
           | Float128Const Rational Position

           | Dot Position
           | Ident String Operator  -- maybe variable name or something
           | Operation Operator Operator