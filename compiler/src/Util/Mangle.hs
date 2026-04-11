module Util.Mangle (
    mangleName,
    mangleNameRaw,
    demangleName,
    demangleNameFromMetaSymbol,
    trimMetaSuffix
    ) where

import Parse.SyntaxTree (Class)

import qualified Util.Basic as Basic


mangleName :: [String] -> [String] -> [Class] -> Bool -> String
mangleName = Basic.mangleName


mangleNameRaw :: [String] -> [String] -> [Class] -> Bool -> String
mangleNameRaw = Basic.mangleNameRaw


demangleName :: String -> Either String ([String], [Class])
demangleName = Basic.demangleName


demangleNameFromMetaSymbol :: String -> Either String ([String], [Class])
demangleNameFromMetaSymbol = Basic.demangleNameFromMetaSymbol


trimMetaSuffix :: String -> String
trimMetaSuffix = Basic.trimMetaSuffix
