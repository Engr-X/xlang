module IR.LowingTest where

import Data.List (find)
import IR.Lowing (codeToIRSingleWithRoot)
import IR.TAC (IRClass (..), IRProgm (..))
import Parse.ParserBasic (DeclFlag (..))
import Parse.SyntaxTree (Class (..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Util.Exception as UE


findAttr :: String -> [((a, [DeclFlag]), Class, String)] -> Maybe ((a, [DeclFlag]), Class, String)
findAttr name = find (\(_, _, attrName) -> attrName == name)


firstWhy :: [UE.ErrorKind] -> Maybe String
firstWhy errors = case errors of
    (UE.Syntax (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Parsing (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Lexer (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    _ -> Nothing


topLevelValFinalTests :: TestTree
topLevelValFinalTests = testGroup "IR.Lowing.topLevelValFinal" $ map (uncurry testCase) [
    ("0", do
        let src = unlines [
                "val pi = 3.14",
                "var n = 1"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ attrs _ _ _ _], _) -> do
                case findAttr "pi" attrs of
                    Nothing -> assertFailure "missing field: pi"
                    Just ((_, flags), cls, _) -> do
                        cls @?= Float64T
                        assertBool "pi should be static" (Static `elem` flags)
                        assertBool "pi should be final" (Final `elem` flags)
                case findAttr "n" attrs of
                    Nothing -> assertFailure "missing field: n"
                    Just ((_, flags), cls, _) -> do
                        cls @?= Int32T
                        assertBool "n should be static" (Static `elem` flags)
                        assertBool "n should not be final" (Final `notElem` flags)
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ),

    ("1", do
        let src = unlines [
                "val x = 1",
                "x = 2"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> firstWhy errs @?= Just (UE.immutableVariableMsg "x")
            Right _ -> assertFailure "expected immutable-variable error"
    ),

    ("2", do
        let src = unlines [
                "int f() {",
                "    val x = 1",
                "    {",
                "        val x = 2",
                "    }",
                "    return x",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right _ -> pure ()
    ),

    ("3", do
        let src = unlines [
                "int f() {",
                "    val x = 1",
                "    val x = 2",
                "    return x",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> firstWhy errs @?= Just (UE.multipleVariableDefMsg "x")
            Right _ -> assertFailure "expected duplicate-variable error"
    )]


tests :: TestTree
tests = testGroup "IR.Lowing" [
    topLevelValFinalTests]
