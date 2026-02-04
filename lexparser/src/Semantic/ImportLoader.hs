module Semantic.ImportLoader where

import Parse.SyntaxTree (Statement, Program, isVariable)
import Semantic.Environment
import Lex.Token (tokenPos)
import Util.Exception (ErrorKind, multipleVariableDefMsg)
import Util.Type (Position, Path)
import Data.Map.Strict (Map)
import Data.List (intercalate)

import qualified Util.Exception as UE
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST


loadSrcI :: Path -> Program -> Either [ErrorKind] ImportEnv
loadSrcI p (dcls, stmts) = case getPackageName p dcls of
    Left errs -> Left errs
    Right packageName -> let (vars, funs, errs) = go packageName stmts in if null errs
        then Right IEnv { file = p, iVars = vars, iFuncs = funs }
        else Left errs
    where
        go :: QName -> [Statement] -> (Map QName [Position], Map (QName, [QName]) [Position], [ErrorKind])
        go _ [] = (Map.empty, Map.empty, [])
        go packageName (stmt : stmts') = let (vars, funs, errs) = go packageName stmts' in case stmt of
            AST.Expr (AST.Binary AST.Assign e _ _) | isVariable e -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in if Map.member fullName vars
                    then (vars, funs, UE.Syntax (UE.makeError p [tokenPos token] (multipleVariableDefMsg s)) : errs)
                    else (Map.insert fullName [tokenPos token] vars, funs, errs)
                AST.Qualified ss tokens -> let name = intercalate "." ss in
                    (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) (multipleVariableDefMsg name)) : errs)

                _ -> (vars, funs, errs)
            _ -> (vars, funs, errs)
