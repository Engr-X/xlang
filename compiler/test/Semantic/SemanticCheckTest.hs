module Semantic.SemanticCheckTest where

import Control.Exception (finally)
import Data.List (isInfixOf)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hPutStr, openTempFile)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Type (Position, makePosition)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Expression(..), Statement(..), Program, Operator(..), prettyExpr)
import Semantic.SemanticCheck
import Semantic.ReturnCheck (funcSig)

import qualified Lex.Token as Lex
import qualified Util.Exception as UE


pos1 :: Position
pos1 = makePosition 1 1 1

pos2 :: Position
pos2 = makePosition 1 2 1

pos3 :: Position
pos3 = makePosition 1 3 1

pos4 :: Position
pos4 = makePosition 1 4 1


tok :: String -> Position -> Lex.Token
tok = Lex.Ident

tokNum :: String -> Position -> Lex.Token
tokNum = Lex.NumberConst

tokSym :: Lex.Symbol -> Position -> Lex.Token
tokSym = Lex.Symbol


intExpr :: Expression
intExpr = IntConst "1" (tokNum "1" pos1)


varExpr :: String -> Position -> Expression
varExpr name p = Variable name (tok name p)


mkFun :: Class -> Lex.Token -> Lex.Token -> Block -> Statement
mkFun retT retTok nameTok body =
    Function (retT, [retTok]) (Variable (tokName nameTok) nameTok) [] body


tokName :: Lex.Token -> String
tokName t = case t of
    Lex.Ident s _ -> s
    _ -> "<name>"


missingMsg :: Class -> Expression -> [(Class, String, [Lex.Token])] -> Maybe [(Class, [Lex.Token])] -> String
missingMsg retT name params mt = UE.missingReturnMsg (funcSig retT name params mt)


firstWhy :: [UE.ErrorKind] -> Maybe String
firstWhy errors = case errors of
    (UE.Syntax (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Parsing (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Lexer (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    _ -> Nothing


assertErrs :: Maybe String -> [UE.ErrorKind] -> Assertion
assertErrs Nothing errs = errs @?= []
assertErrs (Just msg) errs = case firstWhy errs of
    Just whyMsg -> whyMsg @?= msg
    Nothing -> assertFailure $ "unexpected errors: " ++ show errs


checkProgmTests :: TestTree
checkProgmTests = testGroup "Semantic.SemanticCheck.checkProgm" $ map mkCase [
    ("0", progMissingReturn, Just (missingMsg Int32T (varExpr "f" pos2) [] Nothing)),
    ("1", progBadAssign, Just (UE.cannotAssignMsg (prettyExpr 0 (Just (IntConst "1" (tokNum "1" pos1)))))),
    ("2", progBadReturnType, Nothing),
    ("3", progOk, Nothing)
    ]
    where
        retTok = tok "int" pos1
        nameTok = tok "f" pos2

        progMissingReturn :: Program
        progMissingReturn = ([], [
            mkFun Int32T retTok nameTok (Multiple [Expr intExpr])
            ])

        progBadAssign :: Program
        progBadAssign = ([], [
            Expr (Binary Assign (IntConst "1" (tokNum "1" pos1)) (IntConst "2" (tokNum "2" pos2)) (tokSym Lex.Assign pos3))
            ])

        progBadReturnType :: Program
        progBadReturnType = ([], [
            Function (Int32T, [retTok]) (Variable "f" nameTok) [] (Multiple [
                Command (Return (Just (BoolConst True (tok "true" pos1)))) (tok "return" pos1)
                ])
            ])

        progOk :: Program
        progOk = ([], [
            Function (Int32T, [retTok]) (Variable "f" nameTok) [] (Multiple [
                Command (Return (Just intExpr)) (tok "return" pos1)
                ])
            ])

        mkCase (name, prog, errMsg) = testCase name $ do
            case checkProgm "stdin" prog [] [] of
                Right _ -> assertErrs errMsg []
                Left errs -> assertErrs errMsg errs


dumpFullUseMapsTests :: TestTree
dumpFullUseMapsTests = testGroup "Semantic.SemanticCheck.dumpFullUseMapsFromFile" $ map mkCase [
    ("0", Nothing, Nothing),
    ("1", Just "@", Nothing),
    ("2", Just "int f() { }", Nothing),
    ("3", Just "int f() { return 1; }", Just True)
    ]
    where
        mkCase (name, mSrc, expectedHasMarkers) = testCase name $ do
            out <- case mSrc of
                Nothing -> dumpFullUseMapsFromFile "no_such_file_xlang"
                Just src -> withTempFile src dumpFullUseMapsFromFile
            case expectedHasMarkers of
                Nothing -> do
                    assertBool "expected non-empty output" (not (null out))
                    assertBool "should not contain vars header" (not ("[Vars]" `isInfixOf` out))
                Just True -> do
                    assertBool "expected [Vars] header" ("[Vars]" `isInfixOf` out)
                    assertBool "expected [Funs] header" ("[Funs]" `isInfixOf` out)
                Just False ->
                    assertFailure "unexpected Just False in test data"


withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "semantic_check_test.xl"
    hPutStr h content
    hClose h
    action path `finally` removeFile path


tests :: TestTree
tests = testGroup "Semantic.SemanticCheck" [checkProgmTests, dumpFullUseMapsTests]
