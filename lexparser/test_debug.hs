import Lex.Tokenizer
import Lex.AutoSemi

main :: IO ()
main = do
    let input = "if a\n    b = 1"
    let (_, tokens) = debugTokenize input
    putStrLn "Input tokens:"
    mapM_ print tokens
    putStrLn "\nAfter insertSemicolons:"
    let result = insertSemicolons tokens
    mapM_ print result
