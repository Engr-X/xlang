module Semantic.ImportLoader where

import Parse.SyntaxTree (Statement, Program, isVariable, prettyClass)
import Semantic.NameEnv
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..))
import Lex.Token (Token, tokenPos)
import Util.Exception (ErrorKind, multipleVariableDefMsg, unsupportedErrorMsg, assignErrorMsg, unsupportedErrorMsg)
import Util.Type (Position, Path)
import Data.Map.Strict (Map)
import Data.List (intercalate)

import qualified Util.Exception as UE
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST


-- | Load import environment from a source program.
--   Collects top-level variable assignments and function declarations
--   under the package name, or returns accumulated errors.
loadSrcI :: Path -> Program -> Either [ErrorKind] ImportEnv
loadSrcI p (dcls, stmts) = case getPackageName p dcls of
    Left errs -> Left errs
    Right packageName -> let (vars, funs, errs) = go packageName stmts in if null errs
        then Right IEnv { file = p, iVars = vars, iFuncs = funs }
        else Left $ reverse errs
    where
        go :: QName -> [Statement] -> (Map QName [Position], Map QName [Position], [ErrorKind])
        go _ [] = (Map.empty, Map.empty, [])
        go packageName (stmt : stmts') = let (vars, funs, errs) = go packageName stmts' in case stmt of
            AST.Expr (AST.Binary AST.Assign e _ _) | isVariable e -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in if Map.member fullName vars
                    then (vars, funs, UE.Syntax (UE.makeError p [tokenPos token] (multipleVariableDefMsg s)) : errs)
                    else (Map.insert fullName [tokenPos token] vars, funs, errs)
                AST.Qualified _ tokens -> (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) assignErrorMsg) : errs)
                _ -> error "internal error this error should be catched in process of parser"

            AST.Function _ e _ _ -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in (vars, Map.insert fullName [tokenPos token] funs, errs)
                AST.Qualified _ tokens -> (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg) : errs)
                _ -> (vars, funs, errs)
            AST.FunctionT _ e _ _ _ -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in (vars, Map.insert fullName [tokenPos token] funs, errs)
                AST.Qualified _ tokens -> (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg) : errs)
                _ -> (vars, funs, errs)
            _ -> (vars, funs, errs)


-- | Load typed import environment from a source program.
--   Collects top-level variables/functions with (placeholder) types.
loadSrcTypedI :: Path -> Program -> Either [ErrorKind] TypedImportEnv
loadSrcTypedI p (dcls, stmts) = case getPackageName p dcls of
    Left errs -> Left errs
    Right packageName -> let (vars, funs, errs) = go packageName stmts in if null errs
        then Right TIEnv { tFile = p, tVars = vars, tFuncs = funs }
        else Left $ reverse errs
    where
        go :: QName
           -> [Statement]
           -> (Map QName (AST.Class, [Position]), Map QName ([FunSig], [Position]), [ErrorKind])
        go _ [] = (Map.empty, Map.empty, [])
        go packageName (stmt : stmts') =
            let (vars, funs, errs) = go packageName stmts' in case stmt of
                AST.Expr (AST.Binary AST.Assign e _ _) | isVariable e -> case e of
                    AST.Variable s token ->
                        let fullName = packageName ++ [s]
                            pos = tokenPos token
                        in if Map.member fullName vars
                            then (vars, funs, UE.Syntax (UE.makeError p [pos] (multipleVariableDefMsg s)) : errs)
                            else (Map.insert fullName (AST.ErrorClass, [pos]) vars, funs, errs)
                    AST.Qualified _ tokens ->
                        (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) assignErrorMsg) : errs)
                    _ -> error "internal error this error should be catched in process of parser"

                AST.Function (retT, _) e params _ -> case e of
                    AST.Variable s token ->
                        let fullName = packageName ++ [s]
                            pos = tokenPos token
                            sig = FunSig { funParams = map (\(t, _, _) -> t) params
                                          , funReturn = retT }
                            sigText = prettySig s sig params Nothing
                            (funs', newErrs) = addFun fullName sig sigText pos funs
                        in (vars, funs', newErrs ++ errs)
                    AST.Qualified _ tokens ->
                        (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg) : errs)
                    _ -> (vars, funs, errs)

                AST.FunctionT (retT, _) e tparams params _ -> case e of
                    AST.Variable s token ->
                        let fullName = packageName ++ [s]
                            pos = tokenPos token
                            sig = FunSig { funParams = map (\(t, _, _) -> t) params
                                          , funReturn = retT }
                            sigText = prettySig s sig params (Just tparams)
                            (funs', newErrs) = addFun fullName sig sigText pos funs
                        in (vars, funs', newErrs ++ errs)
                    AST.Qualified _ tokens ->
                        (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg) : errs)
                    _ -> (vars, funs, errs)

                _ -> (vars, funs, errs)

        addFun :: QName -> FunSig -> String -> Position ->
            Map QName ([FunSig], [Position]) ->
            (Map QName ([FunSig], [Position]), [ErrorKind])
        addFun name sig sigText pos funs =
            case Map.lookup name funs of
                Nothing -> (Map.insert name ([sig], [pos]) funs, [])
                Just (sigsOld, posOld) ->
                    if any (sameArgs sig) sigsOld
                        then (funs, [UE.Syntax (UE.makeError p [pos] (UE.duplicateMethodMsg sigText))])
                        else (Map.insert name (sig : sigsOld, pos : posOld) funs, [])

        sameArgs :: FunSig -> FunSig -> Bool
        sameArgs a b = funParams a == funParams b

        prettySig :: String -> FunSig -> [(AST.Class, String, [Token])] ->
            Maybe [(AST.Class, [Token])] -> String
        prettySig name sig params mTParams =
            let retS = prettyClass (funReturn sig)
                paramS = intercalate ", " [prettyClass t ++ " " ++ n | (t, n, _) <- params]
                genS = case mTParams of
                    Nothing -> ""
                    Just ts -> "<" ++ intercalate ", " (map (prettyClass . fst) ts) ++ ">"
            in concat [retS, " ", name, genS, "(", paramS, ")"]
