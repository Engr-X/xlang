module Util.CompileJavaPolicyTest where

import CompileJava (
    hasJavaImportPrefix,
    isDefaultStdlibJar,
    isDefaultStdlibMetadataDb,
    isJavaNativeMetadataPath
    )
import Test.Tasty
import Test.Tasty.HUnit


defaultStdlibSelectionTests :: TestTree
defaultStdlibSelectionTests = testGroup "CompileJava.defaultStdlibSelection" [
    testCase "xlang-stdlib.jar is accepted" $
        assertBool "should accept xlang stdlib jar"
            (isDefaultStdlibJar "D:\\xlang\\libs\\java\\xlang-stdlib.jar"),
    testCase "kotlin-stdlib is rejected" $
        assertBool "should reject kotlin stdlib jar"
            (not (isDefaultStdlibJar "D:\\xlang\\libs\\java\\kotlin-stdlib-2.2.21.jar")),
    testCase "annotations jar is rejected" $
        assertBool "should reject annotations jar"
            (not (isDefaultStdlibJar "D:\\xlang\\libs\\java\\annotations-13.0.jar")),
    testCase "jdk8 metadata db is stdlib metadata" $
        assertBool "should accept jdk stdlib metadata db"
            (isDefaultStdlibMetadataDb "D:\\xlang\\libs\\java\\jdk8-stdlib.db")
    ]


javaImportDetectionTests :: TestTree
javaImportDetectionTests = testGroup "CompileJava.javaImportDetection" [
    testCase "no java import" $
        hasJavaImportPrefix ["xlang.io.*", "xlang.math.Math"] @?= False,
    testCase "has java import" $
        hasJavaImportPrefix ["xlang.io.*", "java.lang.Math"] @?= True
    ]


javaNativePathTests :: TestTree
javaNativePathTests = testGroup "CompileJava.javaNativePath" [
    testCase "detect libs/java metadata path" $
        isJavaNativeMetadataPath "D:\\xlang\\libs\\java\\jdk8-stdlib.db" @?= True,
    testCase "detect java-native metadata path" $
        isJavaNativeMetadataPath "D:\\xlang\\libs\\java-native\\jdk-8\\jdk-1.8.0_202.db" @?= True,
    testCase "non java-native lib path is ignored" $
        isJavaNativeMetadataPath "D:\\xlang\\libs\\java\\xlang-stdlib.jar" @?= False
    ]


tests :: TestTree
tests = testGroup "CompileJava.policy" [
    defaultStdlibSelectionTests,
    javaImportDetectionTests,
    javaNativePathTests
    ]
