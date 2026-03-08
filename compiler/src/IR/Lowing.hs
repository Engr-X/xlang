module IR.Lowing (
    codeToIRWithRoot,
    codeToIR,
    codeToIRSingleWithRoot,
    codeToIRSingle,
    lowerWithUses
) where

import Control.Monad.State.Strict (runState)
import Data.Char (toUpper)
import Data.Either (lefts, rights)
import Data.List (partition)
import Data.Map.Strict (Map)
import Lex.Tokenizer (tokenizeWithNL)
import Parse.ParseProgm (parseProgm)
import Parse.ParserBasic (toException)
import Parse.SyntaxTree (Program, getErrorProgram)
import Semantic.CheckProgram (checkProgm)
import Semantic.TypeCheck (tcFullFunUses, tcFullVarUses, tcWarnings)
import Semantic.TypeEnv (FullFunctionTable, FullVarTable)
import Util.Exception (ErrorKind, Warning)
import Util.Type (Path, Position)

import qualified IR.TAC as TAC
import qualified IR.Optimize as Opt
import qualified IR.TACLowing as Low
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeCheck as TC
import qualified Util.Exception as UE


-- | Parse, semantically check, and lower source files under an explicit source root.
codeToIRWithRoot :: Path -> [(Path, String)] -> Either [ErrorKind] ([(Path, TAC.IRProgm)], [Warning])
codeToIRWithRoot root files =
    let parsed = map parseOne files
        parseErrs = concat (lefts parsed)
        progms = rights parsed
    in if not (null parseErrs)
        then Left parseErrs
        else case checkProgm root progms of
            Left errs -> Left errs
            Right ctxs ->
                let ctxMap = Map.fromList ctxs
                    lowered = map (lowerOne ctxMap) progms
                    lowerErrs = concat (lefts lowered)
                    loweredOk = rights lowered
                in if not (null lowerErrs)
                    then Left lowerErrs
                    else
                        let (irs, warnsList) = unzip loweredOk
                        in Right (irs, concat warnsList)
    where
        parseOne :: (Path, String) -> Either [ErrorKind] (Path, Program)
        -- | Parse one source file and map lexer/parser failures to semantic-style errors.
        parseOne (path, code) =
            let (lexErrs, tokens) = tokenizeWithNL path code
            in if not (null lexErrs)
                then Left lexErrs
                else
                    let prog = parseProgm tokens
                        parseErrs = getErrorProgram prog
                    in if null parseErrs
                        then Right (path, prog)
                        else Left (map (toException path) parseErrs)

        lowerOne ::
            Map Path TC.TypeCtx ->
            (Path, Program) ->
            Either [ErrorKind] ((Path, TAC.IRProgm), [Warning])
        -- | Lower one already-checked program with its per-file type context.
        lowerOne ctxMap (path, prog) =
            case Map.lookup path ctxMap of
                Nothing ->
                    Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]
                Just ctx ->
                    let (ir, tacWarns) = lowerWithUses path prog (tcFullVarUses ctx) (tcFullFunUses ctx)
                        warns = tcWarnings ctx ++ tacWarns
                    in Right ((path, ir), warns)


-- | Compatibility wrapper using current directory as source root.
codeToIR :: [(Path, String)] -> Either [ErrorKind] ([(Path, TAC.IRProgm)], [Warning])
codeToIR = codeToIRWithRoot "."


-- | Compatibility wrapper for single-file compilation.
codeToIRSingle :: Path -> String -> Either [ErrorKind] (TAC.IRProgm, [Warning])
codeToIRSingle = codeToIRSingleWithRoot "."


-- | Single-file lowering with explicit source root for package validation.
codeToIRSingleWithRoot :: Path -> Path -> String -> Either [ErrorKind] (TAC.IRProgm, [Warning])
codeToIRSingleWithRoot root path code = do
    (irs, warns) <- codeToIRWithRoot root [(path, code)]
    case irs of
        [(_, ir)] -> Right (ir, warns)
        _ -> Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]



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
                else (:[]) <$> Low.classStmtsLowing pkgSegs mainClassName otherStmts
            pure $ TAC.IRProgm pkgSegs (classIRs ++ extraIRs)
        st0 = TAC.mkTACState vUses fUses
        (ir0, st1) = runState (TAC.runTACM action) st0
        ir = Opt.formateIR ir0
        tacWarns = reverse (TAC.tacWarnings st1)
    in (ir, tacWarns)
    where
        -- | Build the synthetic main class name from file base name.
        toMainClassName :: Path -> String
        toMainClassName p =
            let fileName = takeFileName p
                base = takeWhile (/= '.') fileName
                cap = case base of
                    [] -> "Main"
                    (c:cs) -> toUpper c : cs
            in cap ++ "Xl"

        -- | Cross-platform file-name extraction without depending on System.FilePath.
        takeFileName :: FilePath -> FilePath
        takeFileName p =
            let rev = reverse p
                nameRev = takeWhile (\c -> c /= '/' && c /= '\\') rev
            in reverse nameRev
