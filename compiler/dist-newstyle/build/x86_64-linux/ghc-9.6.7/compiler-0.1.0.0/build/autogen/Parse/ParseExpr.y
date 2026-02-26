{
module Parse.ParseExpr where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token, tokenPos)
import Lex.Tokenizer (tokenize)
import Parse.ParserBasic
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (ErrorKind, makeError, expectedExpression, assignErrorMsg)
import Util.Type (Path, defaultPosition)
import qualified Lex.Token as Lex
import qualified Util.Exception as UE
}
%name parseExpr Start
%tokentype { Token }
%error { parseExprError }
%token
    EOF { t@(Lex.EOF _) }
    KW_TRUE { t@(Lex.Ident "true" _) }
    KW_FALSE { t@(Lex.Ident "false" _) }
    KW_AS { t@(Lex.Ident "as" _) }
    identity { t@(Lex.Ident _ _) }
    number { t@(Lex.NumberConst _ _) }
    character { t@(Lex.CharConst _ _) }
    string { t@(Lex.StrConst _ _) }
    '=' { t@(Lex.Symbol Lex.Assign _) }
    '>' { t@(Lex.Symbol Lex.GreaterThan _) }
    '<' { t@(Lex.Symbol Lex.LessThan _) }
    '==' { t@(Lex.Symbol Lex.Equal _) }
    '!=' { t@(Lex.Symbol Lex.NotEqual _) }
    '!' { t@(Lex.Symbol Lex.BitNot _) }
    '+' { t@(Lex.Symbol Lex.Plus _) }
    '-' { t@(Lex.Symbol Lex.Minus _) }
    '*' { t@(Lex.Symbol Lex.Multiply _) }
    '/' { t@(Lex.Symbol Lex.Divide _) }
    '(' { t@(Lex.Symbol Lex.LParen _) }
    ')' { t@(Lex.Symbol Lex.RParen _) }
    ',' { t@(Lex.Symbol Lex.Comma _) }
    '.' { t@(Lex.Symbol Lex.Dot _) }
    '::' { t@(Lex.Symbol Lex.DoubleColon _) }
%right '='
%left '!=' '=='
%left '>' '<'
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS '!'
%%
Start
    : Expr EOF { $1 }
    ;
-- this qname is reversed
QName :: { ([String], [Token]) }
    : identity { ([identText $1], [$1]) }
    | QName '.' identity { let (xs, ts) = $1 in (identText $3 : xs, $3 : ts) }
    ;
-- this Clazzs is reversed
ClazzList :: { [(Class, [Token])] }
    : Clazz { [$1] }
    | ClazzList ',' Clazz { $3 : $1 }
    ;
Clazz :: { (Class, [Token]) }
    : QName { (normalizeClass $ Class (reverse (fst $1)) [] , snd $1) }
    | QName '::' '<' ClazzList '>' { let reversed = reverse $4 in
        (normalizeClass $ Class (reverse (fst $1)) (map fst reversed),
        concat [snd $1, [$2, $3], concatMap snd reversed, [$5]]) }
    ;
LValue :: { Expression }
    : QName { qnameToExpr $1 };
Atom :: { Expression }
    : KW_TRUE { BoolConst True $1 }
    | KW_FALSE { BoolConst False $1 }
    -- unary operators
    | '!' Atom %prec '!' { Unary BitNot $2 $1 }
    | '!' error %prec '!' { Error [$1] (expectedExpression 1 "!") }
    | '+' Atom %prec UPLUS { Unary UnaryPlus $2 $1}
    | '+' error %prec UPLUS { Error [$1] (expectedExpression 1 "+")}
    | '-' Atom %prec UMINUS { Unary UnaryMinus $2 $1}
    | '-' error %prec UMINUS { Error [$1] (expectedExpression 1 "-") }
    -- parenthesized expression
    | '(' Expr ')' { $2 }
    -- identifiers
    | QName { qnameToExpr $1 }
    -- literals
    | number { fromMaybe (Error [$1] "classifyNumber failed") (classifyNumber (numText $1) $1) }
    | character { CharConst (charVal $1) $1 }
    | string { StringConst (strVal $1) $1 }
    ;
-- this qname is reversed
Arguments :: { [Expression] }
    : Expr { [$1] }
    | Arguments ',' Expr { $3 : $1 }
    ;
ArgumentsOpt :: { [Expression] }
    : '(' ')' { [] }
    | '(' Arguments ')' { reverse $2 }
    ;
GenericParamList :: { [(Class, [Token])] }
    : '::' '<' '>' { [] }
    | '::' '<' ClazzList '>' { reverse $3 }
    ;
Postfix :: { Expression }
    : Atom { $1 }
    | Postfix KW_AS Clazz { Cast $3 $1 $2 }
    | Postfix KW_AS error { Error [$2] (expectedExpression 0 "as") }
    ;
Expr :: { Expression }
    : QName ArgumentsOpt { Call (qnameToExpr $1) $2 }
    | QName GenericParamList ArgumentsOpt { CallT (qnameToExpr $1) $2 $3 }
    | LValue '=' Expr { if isVariable $1 then Binary Assign $1 $3 $2 else Error [$2] assignErrorMsg }
    | LValue '=' error { Error [$2] (expectedExpression 1 "=") }
    --
    | Expr '==' Expr { Binary Equal $1 $3 $2}
    | Expr '==' error { Error [$2] (expectedExpression 1 "==") }
    | Expr '!=' Expr { Binary NotEqual $1 $3 $2}
    | Expr '!=' error { Error [$2] (expectedExpression 1 "!=") }
    | Expr '>' Expr { Binary GreaterThan $1 $3 $2}
    | Expr '>' error { Error [$2] (expectedExpression 1 ">") }
    | Expr '<' Expr { Binary LessThan $1 $3 $2}
    | Expr '<' error { Error [$2] (expectedExpression 1 "<") }
    --
    | Expr '+' Expr { Binary Add $1 $3 $2}
    | Expr '+' error { Error [$2] (expectedExpression 1 "+") }
    | Expr '-' Expr { Binary Sub $1 $3 $2}
    | Expr '-' error { Error [$2] (expectedExpression 1 "-") }
    | Expr '*' Expr { Binary Mul $1 $3 $2}
    | Expr '*' error { Error [$2] (expectedExpression 1 "*") }
    | Expr '/' Expr { Binary Div $1 $3 $2}
    | Expr '/' error { Error [$2] (expectedExpression 1 "/") }
    | Postfix { $1 }
    ;
{
-- | Happy-required error continuation for expression parsing.
--
-- Happy enforces a polymorphic error handler of type:
-- [Token] -> a
--
-- However, our parser produces a concrete 'Expression' as the error result.
-- To bridge this mismatch, we deliberately use 'unsafeCoerce' here to coerce
-- an 'Expression' into the expected polymorphic return type.
--
-- This function should ONLY be used as the 'parseError' hook generated by Happy.
-- Any misuse outside the parser error path is undefined behavior.
parseExprError :: [Token] -> a
parseExprError toks = unsafeCoerce (mkHappyErrorExpr toks)
-- | High-level entry point for lexing + parsing a single expression.
--
-- This function performs the following pipeline:
--
-- 1. Tokenize the input source string with newline inference.
-- 2. If lexical errors exist, return them immediately.
-- 3. Parse the token stream into an 'Expression'.
-- 4. Traverse the resulting AST to collect embedded parse errors.
-- 5. Convert all expression-level errors into unified 'ErrorKind's.
--
-- On success, returns:
-- Right Expression
--
-- On failure, returns:
-- Left [ErrorKind]
--
-- This design intentionally separates:
-- * lexing errors (returned directly)
-- * parsing errors (embedded in AST, then extracted)
lexparseExpr :: Path -> String -> Either [ErrorKind] Expression
lexparseExpr p str = let (errs, tokens) = tokenize p str in case errs of
    [] -> let expr = parseExpr tokens in case getErrorProgram ([], [Expr expr]) of
        [] -> Right expr
        errs -> Left $ map (toException p) errs
    _ -> Left errs
-- | used for debug version
replLexparseExpr :: String -> Either [ErrorKind] Expression
replLexparseExpr = lexparseExpr "stdin"
}
