module IR.IRLowing where

import Control.Monad.State.Strict (runState)
import Data.Char (toUpper)
import Data.List (partition)
import Data.Map.Strict (Map)
import Lex.Tokenizer (tokenizeWithNL)
import Parse.ParseProgm (parseProgm)
import Parse.ParserBasic (toException)
import Parse.SyntaxTree (Program, getErrorProgram)
import Semantic.SemanticCheck (checkProgm)
import Semantic.TypeCheck (tcFullFunUses, tcFullVarUses, tcWarnings)
import Semantic.TypeEnv (FullFunctionTable, FullVarTable)
import Util.Exception (ErrorKind, Warning)
import Util.Type (Path, Position)

import qualified IR.TAC as TAC
import qualified IR.Optimize as Opt
import qualified IR.TACLowing as Low
import qualified Parse.SyntaxTree as AST
import qualified Util.FileHelper as FH


-- | Compile source code into IR with early-exit on lex/parse/semantic errors.
--   Warnings from semantic + TAC lowering are accumulated on success.
codeToIR :: Path -> String -> Either [ErrorKind] (TAC.IRProgm, [Warning])
codeToIR path code =
    let (lexErrs, tokens) = tokenizeWithNL path code
    in if not (null lexErrs)
        then Left lexErrs
        else
            let prog = parseProgm tokens
                parseErrs = getErrorProgram prog
            in if null parseErrs
                then case checkProgm path prog [] [] of
                    Left errs -> Left errs
                    Right ctx ->
                        let (ir, tacWarns) = lowerWithUses path prog (tcFullVarUses ctx) (tcFullFunUses ctx)
                            warns = tcWarnings ctx ++ tacWarns
                        in Right (ir, warns)
                else Left (map (toException path) parseErrs)


debugCodeToIR :: Path -> IO ()
debugCodeToIR path = do
    fileRes <- FH.readFile path
    case fileRes of
        Left err -> print err
        Right code ->
            case codeToIR path code of
                Left errs -> mapM_ print errs
                Right (ir, warns) -> do
                    mapM_ print warns
                    putStrLn (TAC.prettyIRProgm ir)


-- | Lower a parsed program to IR using resolved var/fun uses and format it.
lowerWithUses ::
    Path ->
    Program ->
    Map [Position] FullVarTable ->
    Map [Position] FullFunctionTable ->
    (TAC.IRProgm, [Warning])
lowerWithUses path (decls, stmts) vUses fUses =
    let (classStmts, otherStmts) = partition AST.isClassDeclar stmts
        pkgSegs = case filter AST.isPackageDecl decls of
            (d:_) -> AST.declPath d
            [] -> []
        mainClassName = toMainClassName path
        action = do
            classIRs <- mapM Low.classLowing classStmts
            extraIRs <- if null otherStmts
                then pure []
                else (:[]) <$> Low.classStmtsLowing mainClassName otherStmts
            pure $ TAC.IRProgm pkgSegs (classIRs ++ extraIRs)
        st0 = TAC.mkTACState vUses fUses
        (ir0, st1) = runState (TAC.runTACM action) st0
        ir = Opt.formateIR ir0
        tacWarns = reverse (TAC.tacWarnings st1)
    in (ir, tacWarns)
    where
        toMainClassName :: Path -> String
        toMainClassName p =
            let fileName = takeFileName p
                base = takeWhile (/= '.') fileName
                cap = case base of
                    [] -> "Main"
                    (c:cs) -> toUpper c : cs
            in cap ++ "Xl"

        takeFileName :: FilePath -> FilePath
        takeFileName p =
            let rev = reverse p
                nameRev = takeWhile (\c -> c /= '/' && c /= '\\') rev
            in reverse nameRev
