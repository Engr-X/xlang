module Util.TypeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Util.Type


makePositionTests :: TestTree
makePositionTests = testGroup "Util.Type.makePosition" [
    testCase "0" $ makePosition 3 5 2 @?= Position { line = 3, column = 5, len = 2 },
    testCase "1" $ makePosition 0 0 0 @?= Position { line = 0, column = 0, len = 0 },
    testCase "1" $ makePosition 1 2 3 @?= Position { line = 1, column = 2, len = 3 },
    testCase "1" $ makePosition 1 1 1 @?= Position { line = 1, column = 1, len = 1 }]


positionToTupleTests :: TestTree
positionToTupleTests = testGroup "Util.Type.positionToTuple" [
    testCase "0" $ positionToTuple (makePosition 7 8 9) @?= (7, 8, 9),
    testCase "1" $ positionToTuple (makePosition 1 2 3) @?= (1, 2, 3),
    testCase "1" $ positionToTuple (makePosition 2 2 3) @?= (2, 2, 3),
    testCase "1" $ positionToTuple (makePosition 7 4 4) @?= (7, 4, 4)]


sortPositionsTests :: TestTree
sortPositionsTests = testGroup "Util.Type.sortPositions" $ map (\(n, input, out) -> testCase n $ sortPositions input @=? out) [
    ("0", [], []),
    ("1", [p 1 1 9, p 1 2 9, p 2 1 9], [p 1 1 9, p 1 2 9, p 2 1 9]),
    ("2", [p 2 3 9, p 1 10 9, p 1 2 9, p 2 1 9], [p 1 2 9, p 1 10 9, p 2 1 9, p 2 3 9]),
    ("3", [p 1 1 3, p 1 1 1, p 1 0 9, p 1 1 2], [p 1 0 9, p 1 1 3, p 1 1 1, p 1 1 2])]
    where
        p :: Int -> Int -> Int -> Position
        p = makePosition



tests :: TestTree
tests = testGroup "Util.Type" [makePositionTests, positionToTupleTests]
