module Semantic.ContextCheck where

import Semantic.Environment
import Util.Type (Postion)
import Util.Exception (ErrorKind)

import qualified Parse.SyntaxTree as AST


contextCheck :: Path -> AST.Program -> Either [ErrorKind] (AST.Program, Environment)