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
import Util.Type (Path)
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
    '+' { t@(Lex.Symbol Lex.Plus _) }
    '-' { t@(Lex.Symbol Lex.Minus _) }
    '*' { t@(Lex.Symbol Lex.Multiply _) }
    '/' { t@(Lex.Symbol Lex.Divide _) }
    '(' { t@(Lex.Symbol Lex.LParen _) }
    ')' { t@(Lex.Symbol Lex.RParen _) }
    '.' { t@(Lex.Symbol Lex.Dot _) }
    '{' { t@(Lex.Symbol Lex.LBrace _) }
    '}' { t@(Lex.Symbol Lex.RBrace _) }
    ';' { t@(Lex.Symbol Lex.Semicolon _) }
%right '='
%left '!=' '=='
%left '>' '<'
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS
%%
Sep :: { () }
    : ';' { () }
    | NL { () }
    ;
Seps :: { () }
    : {- empty -} { () }
    | Seps Sep { () }
    ;
Seps1 :: { () }
    : Sep { () }
    | Seps1 Sep { () }
    ;
StmtItem :: { Statement }
    : Stmt Seps1 { $1 }
    ;
StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq { $1 }
    ;
StmtSeq :: { [Statement] }
    : StmtItem { [$1] }
    | StmtSeq StmtItem { $1 ++ [$2] }
    ;
-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr WhileBody { While $2 $3 }
    ;
WhileBody :: { Maybe Block }
    : Block { Just $1 } -- while e { ... } or while e ;
    | NL Stmt { Just (stmtToBlock $2) } -- while e \n stmt (single stmt only)
    | ';' { Just (Multiple []) } -- explicit empty body (optional, but clear)
    ;
-- assembled statement
Stmt :: { Statement }
    : WhileStmt { $1 }
    | Expr { Expr $1 }
    | Block { BlockStmt $1 }
    ;
-- block
Block :: { Block }
    : '{' Seps StmtSeqOpt Seps '}' { Multiple $3 }
    | ';' { Multiple [] }
    ;
MaybeBlock :: { Maybe Block }
    : Block { Just $1 }
    | ';' { Just (Multiple []) }
    | Seps { Just (Multiple []) }
    ;
StmtEOF :: { Statement } : Seps Stmt Seps EOF { $2 };
BlockEOF :: { Block } : Seps Block Seps EOF { $2 };
ProgramEOF :: { Program } : Seps StmtSeqOpt Seps EOF { ([], $2) };
QName
    : identity { ([identText $1], [$1]) }
    | QName '.' identity { let (xs, ts) = $1 in (xs ++ [identText $3], ts ++ [$3]) }
    ;
LValue : QName { qnameToExpr $1 };
Type : QName { let (xs, _) = $1 in toClass (intercalate "." xs) };
Atom
    : KW_TRUE { BoolConst True $1}
    | KW_FALSE { BoolConst False $1}
    -- unary operators
    | '+' Atom %prec UPLUS { Unary UnaryPlus $2 $1}
    | '+' error %prec UPLUS { Error $1 (expectedExpression 1 "+")}
    | '-' Atom %prec UMINUS { Unary UnaryMinus $2 $1}
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }
    -- parenthesized expression
    | '(' Expr ')' { $2 }
    -- identifiers
    | QName { qnameToExpr $1 }
    -- literals
    | number { fromMaybe (Error $1 "classifyNumber failed") (classifyNumber (numText $1) $1) }
    | character { CharConst (charVal $1) $1 }
    | string { StringConst (strVal $1) $1 }
    ;
Postfix
    : Atom { $1 }
    -- cast
    | Postfix KW_AS Type { Cast $3 $1 $2 }
    | Postfix KW_AS error { Error $2 (expectedExpression 0 "as") }
    ;
Expr
    : LValue '=' Expr { if isVariable $1 then Binary Assign $1 $3 $2 else Error $2 assignErrorMsg }
    | LValue '=' error { Error $2 (expectedExpression 1 "=") }
    --
    | Expr '==' Expr { Binary Equal $1 $3 $2}
    | Expr '==' error { Error $2 (expectedExpression 1 "==") }
    | Expr '!=' Expr { Binary NotEqual $1 $3 $2}
    | Expr '!=' error { Error $2 (expectedExpression 1 "!=") }
    | Expr '>' Expr { Binary GreaterThan $1 $3 $2}
    | Expr '>' error { Error $2 (expectedExpression 1 ">") }
    | Expr '<' Expr { Binary LessThan $1 $3 $2}
    | Expr '<' error { Error $2 (expectedExpression 1 "<") }
    --
    | Expr '+' Expr { Binary Add $1 $3 $2}
    | Expr '+' error { Error $2 (expectedExpression 1 "+") }
    | Expr '-' Expr { Binary Sub $1 $3 $2}
    | Expr '-' error { Error $2 (expectedExpression 1 "-") }
    | Expr '*' Expr { Binary Mul $1 $3 $2}
    | Expr '*' error { Error $2 (expectedExpression 1 "*") }
    | Expr '/' Expr { Binary Div $1 $3 $2}
    | Expr '/' error { Error $2 (expectedExpression 1 "/") }
    | Postfix { $1 }
    ;
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
