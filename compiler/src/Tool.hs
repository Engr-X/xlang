module Tool where

import Data.Char (isSpace, toLower)
import Parse.SyntaxTree (Class(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Util.Mangle (mangleName)


data ParsedDecl
    = ParsedField Class [String] [String] String
    | ParsedFunc Class [String] [String] String [Class]


main :: IO ()
main = do
    args <- getArgs
    if null args || any (`elem` ["-h", "--help"]) args
        then putStrLn usage
        else do
            let input = trim (unwords args)
            case parseDecl input of
                Left msg -> do
                    putStrLn ("error: " ++ msg)
                    putStrLn usage
                    exitFailure
                Right decl ->
                    putStrLn (mangleDecl decl)


usage :: String
usage = unlines [
    "xlang-tool <declaration>",
    "",
    "Examples:",
    "  xlang-tool \"int com.wangdi.Class1$Class2.a\"",
    "  xlang-tool \"void com.wangdi.Class1$Class2.fun(int, int)\"",
    "",
    "Notes:",
    "  - Use '$' to express nested classes in owner path.",
    "  - Function mangling appends return type after parameter types."
    ]


mangleDecl :: ParsedDecl -> String
mangleDecl (ParsedField ty pkg cls name) =
    mangleQNameWithSig False (pkg ++ cls ++ [name]) [ty]
mangleDecl (ParsedFunc retTy pkg cls name argTys) =
    mangleQNameWithSig True (pkg ++ cls ++ [name]) (argTys ++ [retTy])


mangleQNameWithSig :: Bool -> [String] -> [Class] -> String
mangleQNameWithSig isFunction qn sigTs = case qn of
    [] -> error "mangleQNameWithSig: empty qname"
    [name] -> mangleName [] [name] sigTs isFunction
    _ -> mangleName (init qn) [last qn] sigTs isFunction


parseDecl :: String -> Either String ParsedDecl
parseDecl raw = do
    let s = trim raw
        (tyTxt, rest0) = break isSpace s
        rest = trim rest0
    if null tyTxt
        then Left "missing leading type"
        else if null rest
            then Left "missing qualified symbol after type"
            else do
                ty <- parseType tyTxt
                if '(' `elem` rest
                    then parseFuncDecl ty rest
                    else parseFieldDecl ty rest


parseFieldDecl :: Class -> String -> Either String ParsedDecl
parseFieldDecl ty qname = do
    (pkg, cls, member) <- parseOwnerAndMember qname
    Right (ParsedField ty pkg cls member)


parseFuncDecl :: Class -> String -> Either String ParsedDecl
parseFuncDecl retTy sigTxt = do
    let s = trim sigTxt
    openIdx <- maybe (Left "function signature missing '('") Right (findIndex '(' s)
    if not (")" `isSuffixOfTrim` s)
        then Left "function signature must end with ')'"
        else do
            let ownerAndName = trim (take openIdx s)
                argsTxt = trim (drop (openIdx + 1) (init s))
            (pkg, cls, member) <- parseOwnerAndMember ownerAndName
            argTys <- if null argsTxt
                then Right []
                else mapM parseType (splitComma argsTxt)
            Right (ParsedFunc retTy pkg cls member argTys)


parseOwnerAndMember :: String -> Either String ([String], [String], String)
parseOwnerAndMember raw = do
    (ownerTxt, member) <- splitLastOn '.' (trim raw)
    if null member
        then Left "empty member name"
        else do
            ownerSegs <- splitNonEmpty '.' ownerTxt
            if null ownerSegs
                then Left "missing owner path"
                else do
                    let pkg = init ownerSegs
                        clsTxt = last ownerSegs
                    clsSegs <- splitNonEmpty '$' clsTxt
                    Right (pkg, clsSegs, member)


parseType :: String -> Either String Class
parseType raw =
    let s = trim raw
        low = map toLower s
    in case low of
        "byte" -> Right Int8T
        "int8" -> Right Int8T
        "short" -> Right Int16T
        "int16" -> Right Int16T
        "int" -> Right Int32T
        "int32" -> Right Int32T
        "long" -> Right Int64T
        "int64" -> Right Int64T
        "float" -> Right Float32T
        "float32" -> Right Float32T
        "double" -> Right Float64T
        "float64" -> Right Float64T
        "float128" -> Right Float128T
        "bool" -> Right Bool
        "boolean" -> Right Bool
        "char" -> Right Char
        "void" -> Right Void
        _ ->
            if '<' `elem` s || '>' `elem` s
                then Left ("generic type is not supported in tool input: " ++ s)
                else Class <$> splitClassQName s <*> pure []


splitClassQName :: String -> Either String [String]
splitClassQName raw = do
    parts <- splitNonEmpty '.' (trim raw)
    pure (concatMap splitDollar parts)
    where
        splitDollar :: String -> [String]
        splitDollar = filter (not . null) . splitOn '$'


splitComma :: String -> [String]
splitComma = map trim . filter (not . null) . splitOn ','


splitLastOn :: Char -> String -> Either String (String, String)
splitLastOn c s =
    case break (== c) (reverse s) of
        (_, []) -> Left ("missing separator '" ++ [c] ++ "' in: " ++ s)
        (revRight, _ : revLeft) -> Right (reverse revLeft, reverse revRight)


splitNonEmpty :: Char -> String -> Either String [String]
splitNonEmpty c s =
    let xs = map trim (splitOn c s)
    in if null xs || any null xs
        then Left ("invalid qualified name: " ++ s)
        else Right xs


splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x : xs)
    | x == c = "" : splitOn c xs
    | otherwise = case splitOn c xs of
        [] -> [[x]]
        (y : ys) -> (x : y) : ys


trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
    where
        dropWhileEnd :: (Char -> Bool) -> String -> String
        dropWhileEnd p = reverse . dropWhile p . reverse


findIndex :: Char -> String -> Maybe Int
findIndex c = go 0
    where
        go _ [] = Nothing
        go i (x : xs)
            | x == c = Just i
            | otherwise = go (i + 1) xs


isSuffixOfTrim :: String -> String -> Bool
isSuffixOfTrim suf s = suf `isSuffixOf` trim s
    where
        isSuffixOf :: Eq a => [a] -> [a] -> Bool
        isSuffixOf xs ys = reverse xs `prefixOf` reverse ys

        prefixOf :: Eq a => [a] -> [a] -> Bool
        prefixOf [] _ = True
        prefixOf _ [] = False
        prefixOf (a : as) (b : bs) = a == b && prefixOf as bs
