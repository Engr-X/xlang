{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Parse.ParseBlock where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token, tokenPos)
import Lex.Tokenizer (tokenizeWithNL)
import Parse.ParserBasic
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (ErrorKind, makeError, expectedExpression, assignErrorMsg)
import Util.Type (Path, defaultPosition)
import qualified Lex.Token as Lex
import qualified Util.Exception as UE
import qualified Control.Monad as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.Tuple as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.7

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn5 (())
        | HappyAbsSyn6 ([Statement])
        | HappyAbsSyn8 ((Maybe Block, Token))
        | HappyAbsSyn9 (Statement)
        | HappyAbsSyn11 ([(Class, String, [Token])])
        | HappyAbsSyn13 (Block)
        | HappyAbsSyn16 (Maybe Block)
        | HappyAbsSyn18 (([String], [Token]))
        | HappyAbsSyn19 ([(Class, [Token])])
        | HappyAbsSyn20 ((Class, [Token]))
        | HappyAbsSyn21 (Expression)
        | HappyAbsSyn23 ([Expression])

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["EOF","NL","KW_TRUE","KW_FALSE","KW_AS","KW_IF","KW_ELSE","KW_WHILE","KW_BREAK","KW_CONTINUE","KW_RETURN","identity","number","character","string","'='","'>'","'<'","'=='","'!='","'!'","'+'","'-'","'*'","'/'","'('","')'","'{'","'}'","';'","','","':'","'::'","'.'","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x14\x00\x00\x00\xec\xff\xff\xff\x00\x00\x00\x00\x03\x00\x00\x00\x4c\x00\x00\x00\x39\x00\x00\x00\x5c\x00\x00\x00\x6f\x01\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x00\x00\x00\x73\x01\x00\x00\x86\x00\x00\x00\x8b\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x01\x00\x00\x94\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x00\x00\x00\x88\x00\x00\x00\x9d\x00\x00\x00\x94\x01\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\xdf\x01\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\x11\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\xc7\x00\x00\x00\xdc\x00\x00\x00\xf1\x00\x00\x00\x06\x01\x00\x00\x1b\x01\x00\x00\x30\x01\x00\x00\x45\x01\x00\x00\x6f\x00\x00\x00\x5a\x01\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x84\x01\x00\x00\xd1\x00\x00\x00\xeb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\xee\xff\xff\xff\x3a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\xcd\x00\x00\x00\x41\x00\x00\x00\xe6\x00\x00\x00\x3a\x02\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x43\x02\x00\x00\x00\x00\x00\x00\x43\x02\x00\x00\x00\x00\x00\x00\x97\x00\x00\x00\x00\x00\x00\x00\x97\x00\x00\x00\x00\x00\x00\x00\x17\x00\x00\x00\xf2\x00\x00\x00\xde\x00\x00\x00\xf4\x00\x00\x00\x6f\x01\x00\x00\xf4\x00\x00\x00\xf4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\x00\x00\x00\x82\x00\x00\x00\x89\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x01\x00\x00\x00\x00\x00\x00\x94\x01\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\xf7\x00\x00\x00\x3a\x02\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\x00\x00\x00\x52\x00\x00\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x00\x00\x00\x00\xf8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xf8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x01\x00\x00\x00\x00\x00\x00\xb0\x01\x00\x00\x00\x00\x00\x00\xbe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x01\x00\x00\xbd\x00\x00\x00\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x01\x00\x00\xed\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xad\x00\x00\x00\xc1\x00\x00\x00\xc2\x00\x00\x00\xef\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\x00\x00\x00\xe2\x00\x00\x00\x00\x00\x00\x00\xf9\x01\x00\x00\xfb\x01\x00\x00\x05\x02\x00\x00\x07\x02\x00\x00\x11\x02\x00\x00\x13\x02\x00\x00\x1d\x02\x00\x00\x1f\x02\x00\x00\x48\x00\x00\x00\x29\x02\x00\x00\x7d\x00\x00\x00\x00\x00\x00\x00\xfe\x00\x00\x00\xf7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x00\x00\x0b\x01\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x01\x00\x00\x15\x01\x00\x00\x0f\x01\x00\x00\x16\x01\x00\x00\xda\x01\x00\x00\x1f\x01\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x02\x00\x00\x00\x00\x00\x00\x35\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\x00\x00\x00\x00\x00\x00\x25\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x00\x00\x00\x00\x00\x00\xcb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfa\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\xe0\xff\xff\xff\xdf\xff\xff\xff\xde\xff\xff\xff\xfc\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xff\xff\xa7\xff\xff\xff\x00\x00\x00\x00\xfd\xff\xff\xff\xd1\xff\xff\xff\xd0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xff\xff\xe6\xff\xff\xff\x00\x00\x00\x00\xd8\xff\xff\xff\xc7\xff\xff\xff\xc6\xff\xff\xff\xc5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\xff\xff\xa5\xff\xff\xff\xc8\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\xcb\xff\xff\xff\xca\xff\xff\xff\xcd\xff\xff\xff\xcc\xff\xff\xff\xcf\xff\xff\xff\xce\xff\xff\xff\xe3\xff\xff\xff\xe4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xff\xff\xdd\xff\xff\xff\xfb\xff\xff\xff\xdc\xff\xff\xff\xd7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\xff\xff\xc2\xff\xff\xff\xba\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xff\xff\xb8\xff\xff\xff\xd4\xff\xff\xff\xbd\xff\xff\xff\xbc\xff\xff\xff\xa9\xff\xff\xff\xa8\xff\xff\xff\xab\xff\xff\xff\xaa\xff\xff\xff\xad\xff\xff\xff\xac\xff\xff\xff\xaf\xff\xff\xff\xae\xff\xff\xff\xb5\xff\xff\xff\xb4\xff\xff\xff\xb7\xff\xff\xff\xb6\xff\xff\xff\xb1\xff\xff\xff\xb0\xff\xff\xff\xb3\xff\xff\xff\xb2\xff\xff\xff\x00\x00\x00\x00\xf4\xff\xff\xff\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\xf0\xff\xff\xff\xf2\xff\xff\xff\xc9\xff\xff\xff\xf1\xff\xff\xff\x00\x00\x00\x00\xef\xff\xff\xff\xdb\xff\xff\xff\xf5\xff\xff\xff\x00\x00\x00\x00\xf3\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\xff\xff\xff\x00\x00\x00\x00\xe8\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\xc1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xff\xff\xc0\xff\xff\xff\xd3\xff\xff\xff\x00\x00\x00\x00\xc3\xff\xff\xff\x00\x00\x00\x00\xe7\xff\xff\xff\xee\xff\xff\xff\xeb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\xff\xff\xf7\xff\xff\xff\xf8\xff\xff\xff\x00\x00\x00\x00\xbf\xff\xff\xff\x00\x00\x00\x00\xe9\xff\xff\xff\xd5\xff\xff\xff\xed\xff\xff\xff\xd3\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x03\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x1c\x00\x00\x00\x1f\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x20\x00\x00\x00\x17\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x1d\x00\x00\x00\x1f\x00\x00\x00\x1f\x00\x00\x00\x21\x00\x00\x00\x21\x00\x00\x00\x03\x00\x00\x00\x1f\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x11\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x12\x00\x00\x00\x1b\x00\x00\x00\x1d\x00\x00\x00\x11\x00\x00\x00\x1f\x00\x00\x00\x1d\x00\x00\x00\x21\x00\x00\x00\x1d\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x1f\x00\x00\x00\x21\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x20\x00\x00\x00\x1d\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x21\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x0d\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x1d\x00\x00\x00\x0f\x00\x00\x00\x1f\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x1c\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x12\x00\x00\x00\x07\x00\x00\x00\x1f\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x12\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x1e\x00\x00\x00\x1d\x00\x00\x00\x0d\x00\x00\x00\x1f\x00\x00\x00\x06\x00\x00\x00\x20\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x07\x00\x00\x00\x0d\x00\x00\x00\x1e\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x14\x00\x00\x00\x0f\x00\x00\x00\x0d\x00\x00\x00\x12\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x08\x00\x00\x00\x1c\x00\x00\x00\x06\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x20\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x08\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x11\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x13\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x13\x00\x00\x00\x08\x00\x00\x00\x1d\x00\x00\x00\x08\x00\x00\x00\x13\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\x1b\x00\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x07\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x00\x00\xff\xff\xff\xff\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\xff\xff\xff\xff\x1b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\xff\xff\xff\xff\x04\x00\x00\x00\x05\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\x00\x00\x1c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x00\x00\x11\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\xff\xff\xff\xff\x04\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\xff\xff\xff\xff\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\xff\xff\xff\xff\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\xff\xff\xff\xff\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\xff\xff\xff\xff\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x0d\x00\x00\x00\x1c\x00\x00\x00\x0d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x0d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x16\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x6b\x00\x00\x00\x6b\x00\x00\x00\x03\x00\x00\x00\x27\x00\x00\x00\x26\x00\x00\x00\x15\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x4b\x00\x00\x00\x82\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x4c\x00\x00\x00\x83\x00\x00\x00\x04\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x6b\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x6b\x00\x00\x00\x15\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x6f\x00\x00\x00\x93\x00\x00\x00\x6d\x00\x00\x00\x6d\x00\x00\x00\x6b\x00\x00\x00\x03\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x81\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x87\x00\x00\x00\x52\x00\x00\x00\x06\x00\x00\x00\x81\x00\x00\x00\x6c\x00\x00\x00\x06\x00\x00\x00\x6d\x00\x00\x00\x06\x00\x00\x00\x53\x00\x00\x00\x45\x00\x00\x00\x03\x00\x00\x00\x6d\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x88\x00\x00\x00\x06\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x6d\x00\x00\x00\x18\x00\x00\x00\xd2\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x15\x00\x00\x00\x43\x00\x00\x00\x79\x00\x00\x00\x45\x00\x00\x00\x1d\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x53\x00\x00\x00\x45\x00\x00\x00\x24\x00\x00\x00\x55\x00\x00\x00\x06\x00\x00\x00\x56\x00\x00\x00\x03\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x7d\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x95\x00\x00\x00\x18\x00\x00\x00\x03\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x58\x00\x00\x00\xff\xff\xff\xff\x9a\x00\x00\x00\x88\x00\x00\x00\x30\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x49\x00\x00\x00\x06\x00\x00\x00\x1d\x00\x00\x00\x32\x00\x00\x00\x7a\x00\x00\x00\x88\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x4f\x00\x00\x00\x55\x00\x00\x00\x47\x00\x00\x00\x7b\x00\x00\x00\x2e\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x1d\x00\x00\x00\x55\x00\x00\x00\x50\x00\x00\x00\x97\x00\x00\x00\x1d\x00\x00\x00\x86\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x55\x00\x00\x00\x83\x00\x00\x00\x84\x00\x00\x00\x3f\x00\x00\x00\x2c\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x7e\x00\x00\x00\x8d\x00\x00\x00\x3e\x00\x00\x00\x68\x00\x00\x00\x7f\x00\x00\x00\x8e\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x68\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x8a\x00\x00\x00\x29\x00\x00\x00\x27\x00\x00\x00\x68\x00\x00\x00\x7f\x00\x00\x00\x2e\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x55\x00\x00\x00\x8e\x00\x00\x00\x84\x00\x00\x00\x45\x00\x00\x00\x66\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x29\x00\x00\x00\x29\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x2c\x00\x00\x00\x2a\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x55\x00\x00\x00\x93\x00\x00\x00\x84\x00\x00\x00\x43\x00\x00\x00\x64\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x55\x00\x00\x00\x4b\x00\x00\x00\x95\x00\x00\x00\x68\x00\x00\x00\x6d\x00\x00\x00\x52\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x68\x00\x00\x00\x91\x00\x00\x00\x62\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x4a\x00\x00\x00\x7a\x00\x00\x00\x72\x00\x00\x00\x06\x00\x00\x00\x72\x00\x00\x00\x90\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x8c\x00\x00\x00\xbf\xff\xff\xff\x1d\x00\x00\x00\x99\x00\x00\x00\x60\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x24\x00\x00\x00\x45\x00\x00\x00\x3f\x00\x00\x00\x34\x00\x00\x00\x4e\x00\x00\x00\x7d\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x77\x00\x00\x00\x76\x00\x00\x00\x74\x00\x00\x00\x75\x00\x00\x00\x5e\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x72\x00\x00\x00\x70\x00\x00\x00\x90\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\xd4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\xff\xff\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x4e\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x12\x00\x00\x00\x33\x00\x00\x00\x24\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x0b\x00\x00\x00\x47\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x0b\x00\x00\x00\x30\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x0b\x00\x00\x00\x73\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x27\x00\x00\x00\x70\x00\x00\x00\x27\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x32\x00\x00\x00\x12\x00\x00\x00\x28\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x66\x00\x00\x00\x12\x00\x00\x00\x64\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x62\x00\x00\x00\x12\x00\x00\x00\x60\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x5e\x00\x00\x00\x12\x00\x00\x00\x5c\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x5a\x00\x00\x00\x12\x00\x00\x00\x58\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x53\x00\x00\x00\x12\x00\x00\x00\x89\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x88\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 90) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68),
        (69 , happyReduce_69),
        (70 , happyReduce_70),
        (71 , happyReduce_71),
        (72 , happyReduce_72),
        (73 , happyReduce_73),
        (74 , happyReduce_74),
        (75 , happyReduce_75),
        (76 , happyReduce_76),
        (77 , happyReduce_77),
        (78 , happyReduce_78),
        (79 , happyReduce_79),
        (80 , happyReduce_80),
        (81 , happyReduce_81),
        (82 , happyReduce_82),
        (83 , happyReduce_83),
        (84 , happyReduce_84),
        (85 , happyReduce_85),
        (86 , happyReduce_86),
        (87 , happyReduce_87),
        (88 , happyReduce_88),
        (89 , happyReduce_89),
        (90 , happyReduce_90)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x04\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x0d\x00\x00\x00\x01\x00\x00\x00\x0d\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x03\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x12\x00\x00\x00\x03\x00\x00\x00\x13\x00\x00\x00\x02\x00\x00\x00\x13\x00\x00\x00\x03\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x14\x00\x00\x00\x04\x00\x00\x00\x15\x00\x00\x00\x01\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\x17\x00\x00\x00\x03\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 37 :: Happy_Prelude.Int
happy_n_nonterms = 24 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 _
         =  HappyAbsSyn5
                 (()
        )

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 _
         =  HappyAbsSyn5
                 (()
        )

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn6
                 ([happy_var_1]
        )
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  1# happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_2 : happy_var_1
        )
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  2# happyReduction_5
happyReduction_5  =  HappyAbsSyn6
                 ([]
        )

happyReduce_6 = happySpecReduce_1  2# happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (reverse happy_var_1
        )
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  3# happyReduction_7
happyReduction_7 _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn8
                 ((Nothing, happy_var_1)
        )
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  3# happyReduction_8
happyReduction_8 (HappyAbsSyn13  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn8
                 ((Just happy_var_2, happy_var_1)
        )
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4# happyReduction_9
happyReduction_9 _
        (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (If happy_var_2 Nothing Nothing (happy_var_1, Nothing)
        )
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4# 4# happyReduction_10
happyReduction_10 ((HappyAbsSyn8  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn21  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (If happy_var_2 Nothing (fst happy_var_4) (happy_var_1, Just $ snd happy_var_4)
        ) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  4# happyReduction_11
happyReduction_11 (HappyAbsSyn13  happy_var_3)
        (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (If happy_var_2 (Just happy_var_3) Nothing (happy_var_1, Nothing)
        )
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4# 4# happyReduction_12
happyReduction_12 ((HappyAbsSyn8  happy_var_4) `HappyStk`
        (HappyAbsSyn13  happy_var_3) `HappyStk`
        (HappyAbsSyn21  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (If happy_var_2 (Just happy_var_3) (fst happy_var_4) (happy_var_1, Just $ snd happy_var_4)
        ) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  5# happyReduction_13
happyReduction_13 _
        (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (While happy_var_2 Nothing Nothing (happy_var_1, Nothing)
        )
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4# 5# happyReduction_14
happyReduction_14 ((HappyAbsSyn8  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn21  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (While happy_var_2 Nothing (fst happy_var_4) (happy_var_1, Just $ snd happy_var_4)
        ) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  5# happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_3)
        (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (While happy_var_2 (Just happy_var_3) Nothing (happy_var_1, Nothing)
        )
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4# 5# happyReduction_16
happyReduction_16 ((HappyAbsSyn8  happy_var_4) `HappyStk`
        (HappyAbsSyn13  happy_var_3) `HappyStk`
        (HappyAbsSyn21  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (While happy_var_2 (Just happy_var_3) (fst happy_var_4) (happy_var_1, Just $ snd happy_var_4)
        ) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  6# happyReduction_17
happyReduction_17 (HappyTerminal happy_var_2)
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn11
                 ([(fst happy_var_1, identText happy_var_2, happy_var_2 : snd happy_var_1)]
        )
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4# 6# happyReduction_18
happyReduction_18 ((HappyTerminal happy_var_4) `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn11  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn11
                 ((fst happy_var_3, identText happy_var_4, happy_var_4 : snd happy_var_3) : happy_var_1
        ) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  7# happyReduction_19
happyReduction_19 _
        _
         =  HappyAbsSyn11
                 ([]
        )

happyReduce_20 = happySpecReduce_3  7# happyReduction_20
happyReduction_20 _
        (HappyAbsSyn11  happy_var_2)
        _
         =  HappyAbsSyn11
                 (reverse happy_var_2
        )
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  8# happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn13
                 (happy_var_1
        )
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  8# happyReduction_22
happyReduction_22 _
        (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn13
                 (Multiple [Command (Return (Just happy_var_2)) happy_var_1]
        )
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4# 9# happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_4) `HappyStk`
        (HappyAbsSyn11  happy_var_3) `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        (HappyAbsSyn20  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Function happy_var_1 (qnameToExpr happy_var_2) happy_var_3 happy_var_4
        ) `HappyStk` happyRest

happyReduce_24 = happyReduce 5# 9# happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_5) `HappyStk`
        (HappyAbsSyn11  happy_var_4) `HappyStk`
        (HappyAbsSyn19  happy_var_3) `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        (HappyAbsSyn20  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (FunctionT happy_var_1 (qnameToExpr happy_var_2) happy_var_3 happy_var_4 happy_var_5
        ) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  10# happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (Command Continue happy_var_1
        )
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  10# happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (Command Break happy_var_1
        )
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  10# happyReduction_27
happyReduction_27 _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (Command (Return Nothing) happy_var_1
        )
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  10# happyReduction_28
happyReduction_28 (HappyAbsSyn9  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (Command (Return (Just $ stmtToExpr happy_var_1 happy_var_2)) happy_var_1
        )
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  10# happyReduction_29
happyReduction_29 _
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn9
                 (Expr happy_var_1
        )
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  10# happyReduction_30
happyReduction_30 _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn9
                 (BlockStmt $ fromMaybe (Multiple []) happy_var_1
        )
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  10# happyReduction_31
happyReduction_31 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  10# happyReduction_32
happyReduction_32 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  10# happyReduction_33
happyReduction_33 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  11# happyReduction_34
happyReduction_34 _
        (HappyAbsSyn6  happy_var_2)
        _
         =  HappyAbsSyn16
                 (Just $ Multiple happy_var_2
        )
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  11# happyReduction_35
happyReduction_35 _
        _
        _
         =  HappyAbsSyn16
                 (Nothing
        )

happyReduce_36 = happySpecReduce_2  12# happyReduction_36
happyReduction_36 (HappyAbsSyn9  happy_var_2)
        _
         =  HappyAbsSyn13
                 (Multiple [happy_var_2]
        )
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  12# happyReduction_37
happyReduction_37 _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn13
                 (fromMaybe (Multiple []) happy_var_1
        )
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  12# happyReduction_38
happyReduction_38 _
        (HappyAbsSyn16  happy_var_2)
        _
         =  HappyAbsSyn13
                 (fromMaybe (Multiple []) happy_var_2
        )
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  13# happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
         =  HappyAbsSyn18
                 (([identText happy_var_1], [happy_var_1])
        )
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  13# happyReduction_40
happyReduction_40 (HappyTerminal happy_var_3)
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (let (xs, ts) = happy_var_1 in (identText happy_var_3 : xs, happy_var_3 : ts)
        )
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  14# happyReduction_41
happyReduction_41 (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn19
                 ([happy_var_1]
        )
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  14# happyReduction_42
happyReduction_42 (HappyAbsSyn20  happy_var_3)
        _
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn19
                 (happy_var_3 : happy_var_1
        )
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  15# happyReduction_43
happyReduction_43 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn20
                 ((normalizeClass $ Class (reverse (fst happy_var_1)) [] , snd happy_var_1)
        )
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happyReduce 5# 15# happyReduction_44
happyReduction_44 ((HappyTerminal happy_var_5) `HappyStk`
        (HappyAbsSyn19  happy_var_4) `HappyStk`
        (HappyTerminal happy_var_3) `HappyStk`
        (HappyTerminal happy_var_2) `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn20
                 (let reversed = reverse happy_var_4 in
        (normalizeClass $ Class (reverse (fst happy_var_1)) (map fst reversed),
        concat [snd happy_var_1, [happy_var_2, happy_var_3], concatMap snd reversed, [happy_var_5]])
        ) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn21
                 (qnameToExpr happy_var_1
        )
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  17# happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (BoolConst True happy_var_1
        )
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  17# happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (BoolConst False happy_var_1
        )
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  17# happyReduction_48
happyReduction_48 (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (Unary BitNot happy_var_2 happy_var_1
        )
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  17# happyReduction_49
happyReduction_49 _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (Error [happy_var_1] (expectedExpression 1 "!")
        )
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  17# happyReduction_50
happyReduction_50 (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (Unary UnaryPlus happy_var_2 happy_var_1
        )
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  17# happyReduction_51
happyReduction_51 _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (Error [happy_var_1] (expectedExpression 1 "+")
        )
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  17# happyReduction_52
happyReduction_52 (HappyAbsSyn21  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (Unary UnaryMinus happy_var_2 happy_var_1
        )
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  17# happyReduction_53
happyReduction_53 _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (Error [happy_var_1] (expectedExpression 1 "-")
        )
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  17# happyReduction_54
happyReduction_54 _
        (HappyAbsSyn21  happy_var_2)
        _
         =  HappyAbsSyn21
                 (happy_var_2
        )
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  17# happyReduction_55
happyReduction_55 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn21
                 (qnameToExpr happy_var_1
        )
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  17# happyReduction_56
happyReduction_56 (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (fromMaybe (Error [happy_var_1] "classifyNumber failed") (classifyNumber (numText happy_var_1) happy_var_1)
        )
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  17# happyReduction_57
happyReduction_57 (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (CharConst (charVal happy_var_1) happy_var_1
        )
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  17# happyReduction_58
happyReduction_58 (HappyTerminal happy_var_1)
         =  HappyAbsSyn21
                 (StringConst (strVal happy_var_1) happy_var_1
        )
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  18# happyReduction_59
happyReduction_59 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn23
                 ([happy_var_1]
        )
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  18# happyReduction_60
happyReduction_60 (HappyAbsSyn21  happy_var_3)
        _
        (HappyAbsSyn23  happy_var_1)
         =  HappyAbsSyn23
                 (happy_var_3 : happy_var_1
        )
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  19# happyReduction_61
happyReduction_61 _
        _
         =  HappyAbsSyn23
                 ([]
        )

happyReduce_62 = happySpecReduce_3  19# happyReduction_62
happyReduction_62 _
        (HappyAbsSyn23  happy_var_2)
        _
         =  HappyAbsSyn23
                 (reverse happy_var_2
        )
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  20# happyReduction_63
happyReduction_63 _
        _
        _
         =  HappyAbsSyn19
                 ([]
        )

happyReduce_64 = happyReduce 4# 20# happyReduction_64
happyReduction_64 (_ `HappyStk`
        (HappyAbsSyn19  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn19
                 (reverse happy_var_3
        ) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_1  21# happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (happy_var_1
        )
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  21# happyReduction_66
happyReduction_66 (HappyAbsSyn20  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Cast happy_var_3 happy_var_1 happy_var_2
        )
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  21# happyReduction_67
happyReduction_67 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 0 "as")
        )
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  22# happyReduction_68
happyReduction_68 (HappyAbsSyn23  happy_var_2)
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn21
                 (Call (qnameToExpr happy_var_1) happy_var_2
        )
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  22# happyReduction_69
happyReduction_69 (HappyAbsSyn23  happy_var_3)
        (HappyAbsSyn19  happy_var_2)
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn21
                 (CallT (qnameToExpr happy_var_1) happy_var_2 happy_var_3
        )
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  22# happyReduction_70
happyReduction_70 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (if isVariable happy_var_1 then Binary Assign happy_var_1 happy_var_3 happy_var_2 else Error [happy_var_2] assignErrorMsg
        )
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  22# happyReduction_71
happyReduction_71 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "=")
        )
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  22# happyReduction_72
happyReduction_72 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary Equal happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  22# happyReduction_73
happyReduction_73 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "==")
        )
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  22# happyReduction_74
happyReduction_74 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary NotEqual happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  22# happyReduction_75
happyReduction_75 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "!=")
        )
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  22# happyReduction_76
happyReduction_76 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary GreaterThan happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  22# happyReduction_77
happyReduction_77 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 ">")
        )
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  22# happyReduction_78
happyReduction_78 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary LessThan happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  22# happyReduction_79
happyReduction_79 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "<")
        )
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  22# happyReduction_80
happyReduction_80 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary Add happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  22# happyReduction_81
happyReduction_81 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "+")
        )
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  22# happyReduction_82
happyReduction_82 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary Sub happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  22# happyReduction_83
happyReduction_83 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "-")
        )
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  22# happyReduction_84
happyReduction_84 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary Mul happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  22# happyReduction_85
happyReduction_85 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "*")
        )
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  22# happyReduction_86
happyReduction_86 (HappyAbsSyn21  happy_var_3)
        (HappyTerminal happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Binary Div happy_var_1 happy_var_3 happy_var_2
        )
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  22# happyReduction_87
happyReduction_87 _
        (HappyTerminal happy_var_2)
        _
         =  HappyAbsSyn21
                 (Error [happy_var_2] (expectedExpression 1 "/")
        )
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  22# happyReduction_88
happyReduction_88 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (happy_var_1
        )
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_2  23# happyReduction_89
happyReduction_89 _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn13
                 (fromMaybe (Multiple []) happy_var_1
        )
happyReduction_89 _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  23# happyReduction_90
happyReduction_90 _
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn13
                 (fromMaybe (Multiple []) happy_var_1
        )
happyReduction_90 _ _ _  = notHappyAtAll 

happyTerminalToTok term = case term of {
        t@(Lex.EOF _) -> 2#;
        t@(Lex.TokenPass _) -> 3#;
        t@(Lex.Ident "true" _) -> 4#;
        t@(Lex.Ident "false" _) -> 5#;
        t@(Lex.Ident "as" _) -> 6#;
        t@(Lex.Ident "if" _) -> 7#;
        t@(Lex.Ident "else" _) -> 8#;
        t@(Lex.Ident "while" _) -> 9#;
        t@(Lex.Ident "break" _) -> 10#;
        t@(Lex.Ident "continue" _) -> 11#;
        t@(Lex.Ident "return" _) -> 12#;
        t@(Lex.Ident _ _) -> 13#;
        t@(Lex.NumberConst _ _) -> 14#;
        t@(Lex.CharConst _ _) -> 15#;
        t@(Lex.StrConst _ _) -> 16#;
        t@(Lex.Symbol Lex.Assign _) -> 17#;
        t@(Lex.Symbol Lex.GreaterThan _) -> 18#;
        t@(Lex.Symbol Lex.LessThan _) -> 19#;
        t@(Lex.Symbol Lex.Equal _) -> 20#;
        t@(Lex.Symbol Lex.NotEqual _) -> 21#;
        t@(Lex.Symbol Lex.BitNot _) -> 22#;
        t@(Lex.Symbol Lex.Plus _) -> 23#;
        t@(Lex.Symbol Lex.Minus _) -> 24#;
        t@(Lex.Symbol Lex.Multiply _) -> 25#;
        t@(Lex.Symbol Lex.Divide _) -> 26#;
        t@(Lex.Symbol Lex.LParen _) -> 27#;
        t@(Lex.Symbol Lex.RParen _) -> 28#;
        t@(Lex.Symbol Lex.LBrace _) -> 29#;
        t@(Lex.Symbol Lex.RBrace _) -> 30#;
        t@(Lex.Symbol Lex.Semicolon _) -> 31#;
        t@(Lex.Symbol Lex.Comma _) -> 32#;
        t@(Lex.Symbol Lex.Colon _) -> 33#;
        t@(Lex.Symbol Lex.DoubleColon _) -> 34#;
        t@(Lex.Symbol Lex.Dot _) -> 35#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 36# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 36# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\tks -> resume (Happy_Prelude.tail tks))


newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Happy_Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Happy_Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => (HappyIdentity a) -> (a -> (HappyIdentity b)) -> (HappyIdentity b)
happyThen = (Happy_Prelude.>>=)
happyReturn :: () => a -> (HappyIdentity a)
happyReturn = (Happy_Prelude.return)
happyThen1 m k tks = (Happy_Prelude.>>=) m (\a -> k a tks)
happyFmap1 f m tks = happyThen (m tks) (\a -> happyReturn (f a))
happyReturn1 :: () => a -> b -> (HappyIdentity a)
happyReturn1 = \a tks -> (Happy_Prelude.return) a
happyReport' :: () => [(Token)] -> [Happy_Prelude.String] -> ([(Token)] -> (HappyIdentity a)) -> (HappyIdentity a)
happyReport' = (\tokens expected resume -> HappyIdentity Happy_Prelude.$ (parseBlockError) tokens)

happyAbort :: () => [(Token)] -> (HappyIdentity a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

parseBlock tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyDoParse 0# tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Happy 2.0 expects a polymorphic error handler.
-- We return a sentinel block containing a single error expression statement.
parseBlockError :: [Token] -> a
parseBlockError toks =
    let e = mkHappyErrorExpr toks
        bl = Multiple [Expr e] in unsafeCoerce bl
-- parse for block into ast tree
lexparseBlock :: Path -> String -> Either [ErrorKind] Block
lexparseBlock p str =
    let (errs, tokens) = tokenizeWithNL p str
    in case errs of
        [] ->
            let blk = parseBlock tokens
            in case getErrorProgram ([], [BlockStmt blk]) of
                [] -> Right blk
                es -> Left $ map (toException p) es
        _ -> Left errs
-- | used for debug version
replLexparseBlock :: String -> Either [ErrorKind] Block
replLexparseBlock = lexparseBlock "stdin"
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyDoParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
