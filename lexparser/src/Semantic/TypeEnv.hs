module Semantic.TypeEnv where

import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..), prettyClass)
import Semantic.NameEnv (QName)
import Util.Type (Position)

import qualified Data.Map.Strict as Map


