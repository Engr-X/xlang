{-# LANGUAGE PatternSynonyms #-}

module IR.LowingTest where

import Data.List (find)
import IR.Lowing (codeToIRSingleWithRoot)
import IR.TAC (IRAtom(..), IRBlock (..), IRClass (..), IRFunction (..), IRInstr (..), IRProgm (..))
import Parse.SyntaxTree (DeclFlag (..))
import Parse.SyntaxTree (Class (..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Util.Exception as UE


findAttr ::
    String ->
    [((a, [DeclFlag]), Class, String, b)] ->
    Maybe ((a, [DeclFlag]), Class, String, b)
findAttr name = find (\(_, _, attrName, _) -> attrName == name)


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
            Right (IRProgm _ [IRClass _ _ _ attrs _ _ _ _ _ _], _) -> do
                case findAttr "pi" attrs of
                    Nothing -> assertFailure "missing field: pi"
                    Just ((_, flags), cls, _, _) -> do
                        cls @?= Float64T
                        assertBool "pi should be static" (Static `elem` flags)
                        assertBool "pi should be final" (Final `elem` flags)
                case findAttr "n" attrs of
                    Nothing -> assertFailure "missing field: n"
                    Just ((_, flags), cls, _, _) -> do
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
    )]

templateInstantiationLoweringTests :: TestTree
templateInstantiationLoweringTests = testGroup "IR.Lowing.templateInstantiation" [
    testCase "template call should instantiate concrete function and call it in IR" $ do
        let src = unlines [
                "fun add<T>(a: T, b: T) -> T = a;",
                "fun main() -> int {",
                "    return add<int>(1L, 2L)",
                "}"
                ]
            collectInstrs :: [IRBlock] -> [IRInstr]
            collectInstrs = concatMap (\(IRBlock (_, body)) -> body)
            hasAddCall :: [IRInstr] -> Bool
            hasAddCall = any isAddCall
                where
                    isAddCall instr = case instr of
                        ICallStatic _ qname _ -> not (null qname) && last qname == "add"
                        ICallVirtual _ qname _ -> not (null qname) && last qname == "add"
                        ICallStaticDirect _ qname _ -> not (null qname) && last qname == "add"
                        _ -> False

        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ _ funs _ _ _], _) -> do
                assertBool "should generate concrete add function"
                    (any (\(IRFunction _ name _ _ _ _) -> name == "add") funs)
                case find (\(IRFunction _ name _ _ _ _) -> name == "main") funs of
                    Nothing -> assertFailure "missing function main"
                    Just (IRFunction _ _ _ _ blocks _) ->
                        assertBool "main should call concrete add function"
                            (hasAddCall (collectInstrs (fst blocks)))
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]

nestedFunctionLoweringTests :: TestTree
nestedFunctionLoweringTests = testGroup "IR.Lowing.nestedFunction" [
    testCase "nested functions should be hoisted before TAC lowering" $ do
        let src = unlines [
                "public fun add(a: int, b: int) -> int {",
                "    fun inner1(x: int) -> int {",
                "        fun inner2(y: int) -> int {",
                "            return x + y",
                "        }",
                "        return inner2(b)",
                "    }",
                "    return inner1(a)",
                "}",
                "public fun main() -> int {",
                "    return add(2, 3)",
                "}"
                ]
        case codeToIRSingleWithRoot "." "Main.x" src of
            Left errs -> assertFailure ("unexpected errors: " ++ show errs)
            Right (IRProgm _ [IRClass _ _ _ _ _ _ funs _ _ _], _) -> do
                assertBool "outer function should remain" (any isAdd funs)
                assertBool "hoisted nested functions should be generated" (any isGenerated funs)
            Right (ir, _) -> assertFailure ("unexpected ir shape: " ++ show ir)
    ]
    where
        isAdd :: IRFunction -> Bool
        isAdd (IRFunction _ "add" _ _ _ _) = True
        isAdd _ = False

        isGenerated :: IRFunction -> Bool
        isGenerated (IRFunction _ name _ _ _ _) = '$' `elem` name


tests :: TestTree
tests = testGroup "IR.Lowing" [
    topLevelValFinalTests,
    templateInstantiationLoweringTests,
    nestedFunctionLoweringTests]

