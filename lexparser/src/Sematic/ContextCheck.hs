module Sematic.ContextCheck where

import Util.Type (Postion)
import Util.Exception (ErrorKind)

import qualified Parse.SyntaxTree as AST


-- Stable unique id for a symbol definition.
newtype VarId = VarId Int deriving (Eq, Ord, Show)

-- Use Position (from tokenPos) as occurrence key.
type RefKey = [Position]     -- or whatever tokenPos returns in your codebase
type Bindings = Map RefKey VarId



-- contextCheck :: Path -> AST.Program -> Either [ErrorKind] AST.Program