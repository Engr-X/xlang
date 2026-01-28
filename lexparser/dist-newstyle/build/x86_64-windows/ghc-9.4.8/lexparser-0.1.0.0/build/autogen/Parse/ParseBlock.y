{
module Parse.ParseBlock where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token, tokenPos)
import Lex.Tokenizer (tokenizeWithNL)
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (ErrorKind, makeError, expectedExpression, assignErrorMsg)
import Util.Type (Path, defaultPosition)
import qualified Lex.Token as Lex
import qualified Util.Exception as UE
}
%name parseBlock BlockEOF
%tokentype { Token }
%error { parseBlockError }
%token
    -- keywords
    EOF { t@(Lex.EOF _) }
    NL { t@(Lex.NewLine _) }
    KW_TRUE { t@(Lex.Ident "true" _) }
    KW_FALSE { t@(Lex.Ident "false" _) }
    KW_AS { t@(Lex.Ident "as" _) }
    KW_IF { t@(Lex.Ident "if" _) }
    KW_ELSE { t@(Lex.Ident "else" _) }
    KW_WHILE { t@(Lex.Ident "while" _) }
    KW_IMPORT { t@(Lex.Ident "import" _) }
    KW_BREAK { t@(Lex.Ident "break" _) }
    KW_CONTINUE { t@(Lex.Ident "continue" _) }
    KW_RETURN { t@(Lex.Ident "return" _) }
    -- literals / identifiers
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
    '{' { t@(Lex.Symbol Lex.LBrace _) }
    '}' { t@(Lex.Symbol Lex.RBrace _) }
    ';' { t@(Lex.Symbol Lex.Semicolon _) }
    ',' { t@(Lex.Symbol Lex.Comma _) }
    ':' { t@(Lex.Symbol Lex.Colon _) }
    '.' { t@(Lex.Symbol Lex.Dot _) }
%right '='
%left '!=' '=='
%left '>' '<'
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS '!'
%nonassoc IFX
%nonassoc KW_ELSE
%%
QName2 :: { ([String], [Token]) }
    : identity { ([identText $1], [$1]) }
    | QName2 '.' identity { let (xs, ts) = $1 in (identText $3 : xs, $3 : ts) }
    ;
Sep :: { () }
    : ';' { () }
    | NL { () }
    ;
StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq { $1 }
    ;
StmtSeq :: { [Statement] }
    : Stmt { [$1] }
    | StmtSeq Stmt { $1 ++ [$2] }
    ;
ElseBlock :: { Maybe Block }
    : KW_ELSE ';' { Nothing }
    | KW_ELSE ':' Stmt { Just $ stmtToBlock $3 }
    ;
IfStmt :: { Statement }
    : KW_IF Expr ';' %prec IFX { If $2 Nothing Nothing }
    | KW_IF Expr ';' ElseBlock { If $2 Nothing $4 }
    | KW_IF Expr ':' Stmt %prec IFX { If $2 (Just $ stmtToBlock $4) Nothing }
    | KW_IF Expr ':' Stmt ElseBlock { If $2 (Just $ stmtToBlock $4) $5 }
    ;
-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';' %prec IFX { While $2 Nothing Nothing }
    | KW_WHILE Expr ';' ElseBlock { While $2 Nothing $4 }
    | KW_WHILE Expr ':' Stmt %prec IFX { While $2 (Just $ stmtToBlock $4) Nothing }
    | KW_WHILE Expr ':' Stmt ElseBlock { While $2 (Just $ stmtToBlock $4) $5 }
    ;
-- assembled statement
Stmt :: { Statement }
    : KW_IMPORT QName2 { Command (Import $ fst $2) $1 }
    | KW_CONTINUE { Command Continue $1 }
    | KW_BREAK { Command Break $1 }
    | KW_RETURN ';' { Command (Return Nothing) $1 }
    | KW_RETURN Stmt { Command (Return (Just $ stmtToExpr $1 $2)) $1 }
    | Expr Sep { Expr $1 }
    | Block { BlockStmt $ fromMaybe (Multiple []) $1 }
    | IfStmt { $1 }
    | WhileStmt { $1 }
    ;
-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}' Sep { Just $ Multiple $2 }
    | '{' Sep '}' Sep { Nothing }
    ;
-- this qname is reversed
QName :: { ([String], [Token]) }
    : identity { ([identText $1], [$1]) }
    | QName '.' identity { let (xs, ts) = $1 in (identText $3 : xs, $3 : ts) }
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
Postfix :: { Expression }
    : Atom { $1 }
    -- cast
    | Postfix KW_AS QName { Cast (toClass (fst $3), snd $3) $1 $2 }
    | Postfix KW_AS error { Error [$2] (expectedExpression 0 "as") }
    ;
Expr :: { Expression }
    : LValue '=' Expr { if isVariable $1 then Binary Assign $1 $3 $2 else Error [$2] assignErrorMsg }
    | LValue '=' error { Error [$2] (expectedExpression 1 "=") }
    | QName '(' ')' { let (ss, ts) = $1 in Call (reverse ss) [] (reverse ts) }
    | QName '(' Arguments ')' { let (ss, ts) = $1 in Call (reverse ss) $3 (reverse ts) }
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
BlockEOF :: { Block } : Block EOF { fromMaybe (Multiple []) $1 };
{
-- Happy 2.0 expects a polymorphic error handler.
-- We return a sentinel block containing a single error expression statement.
parseBlockError :: [Token] -> a
parseBlockError toks =
    let e = mkHappyErrorExpr toks
        bl = Multiple [Expr e] in unsafeCoerce bl
-- parse for block into ast tree
lexparseBlock :: Path -> String -> Either [ErrorKind] Block
lexparseBlock p str =
    let (errs, tokens) = tokenizeWithNL p str
    in case errs of
        [] ->
            let blk = parseBlock tokens
            in case getErrorProgram ([], [BlockStmt blk]) of
                [] -> Right blk
                es -> Left $ map (toException p) es
        _ -> Left errs
-- | used for debug version
replLexparseBlock :: String -> Either [ErrorKind] Block
replLexparseBlock = lexparseBlock "stdin"
}
