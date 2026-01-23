{
module Parse.ParseProgm where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token)
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (expectedExpression)
import qualified Lex.Token as Lex
}
%name parseProgm ProgramEOF
%tokentype { Token }
%error { parseProgmError }
%token
    -- keywords (Ident)
    KW_TRUE { Lex.Ident "true" _ }
    KW_FALSE { Lex.Ident "false" _ }
    KW_AS { Lex.Ident "as" _ }
    -- literals / identifiers
    identity { Lex.Ident $$ _ }
    number { Lex.NumberConst $$ _ }
    character { Lex.CharConst $$ _ }
    string { Lex.StrConst $$ _ }
    -- newline / eof
    NL { Lex.NewLine _ }
    EOF { Lex.EOF _ }
    -- symbols (Expr)
    '.' { Lex.Symbol Lex.Dot _ }
    '+' { Lex.Symbol Lex.Plus _ }
    '-' { Lex.Symbol Lex.Minus _ }
    '*' { Lex.Symbol Lex.Multiply _ }
    '/' { Lex.Symbol Lex.Divide _ }
    '(' { Lex.Symbol Lex.LParen _ }
    ')' { Lex.Symbol Lex.RParen _ }
    -- symbols (Stmt/Block)
    '{' { Lex.Symbol Lex.LBrace _ }
    '}' { Lex.Symbol Lex.RBrace _ }
    ';' { Lex.Symbol Lex.Semicolon _ }
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS
%%
ProgramEOF :: { Program } : OptTerms StmtSeq OptTerms EOF { ([], $2) }
StmtSeq :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq OptTerms Stmt { $1 ++ [$3] }
    ;
-- separators
Term :: { () }
    : ';' { () }
    | NL { () }
OptTerms :: { () }
    : {- empty -} { () }
    | OptTerms Term { () }
Stmt :: { Statement }
    : Expr { Expr $1 }
    | Block { BlockStmt $1 }
    ;
-- block\U0000ff08\U00005982\U0000679c Stmt \U000091cc\U00005f15\U00007528\U00004e86 Block\U0000ff0cBlock \U00004e5f\U00005fc5\U0000987b\U00005b58\U00005728\U0000ff09
Block :: { Block }
    : '{' OptTerms BlockBody OptTerms '}' { Multiple $3 }
BlockBody :: { [Statement] }
    : {- empty -} { [] }
    | BlockBody OptTerms Stmt { $1 ++ [$3] }
-- ============================================================
-- Expression grammar (shared by Expr / Stmt / Block / Program)
-- ============================================================
QName
    : identity { [$1] }
    | QName '.' identity { $1 ++ [$3] }
    ;
Type
    : QName { toClass (intercalate "." $1) }
    ;
Atom
    : KW_TRUE { BoolConst True }
    | KW_FALSE { BoolConst False }
    -- unary operators
    | '+' Atom %prec UPLUS { Unary UnaryPlus $2 }
    | '+' error %prec UPLUS { Error $1 (expectedExpression 1 "+") }
    | '-' Atom %prec UMINUS { Unary UnaryMinus $2 }
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }
    -- parenthesized expression
    | '(' Expr ')' { $2 }
    -- identifiers
    | QName { qnameToExpr $1 }
    -- literals
    | number { fromMaybe (error "classifyNumber failed")
                                      (classifyNumber $1) }
    | character { CharConst $1 }
    | string { StringConst $1 }
    ;
Postfix
    : Atom { $1 }
    -- cast
    | Postfix KW_AS Type { Cast $3 $1 }
    | Postfix KW_AS error { Error $2 (expectedExpression 0 "as") }
    ;
Expr
    : Expr '+' Expr { Binary Add $1 $3 }
    | Expr '+' error { Error $2 (expectedExpression 1 "+") }
    | Expr '-' Expr { Binary Sub $1 $3 }
    | Expr '-' error { Error $2 (expectedExpression 1 "-") }
    | Expr '*' Expr { Binary Mul $1 $3 }
    | Expr '*' error { Error $2 (expectedExpression 1 "*") }
    | Expr '/' Expr { Binary Div $1 $3 }
    | Expr '/' error { Error $2 (expectedExpression 1 "/") }
    | Postfix { $1 }
    ;
{
-- Happy expects a polymorphic error handler.
-- We return a sentinel program containing one error expression statement.
parseProgmError :: [Token] -> a
parseProgmError toks =
    let e = mkHappyErrorExpr toks
        pr = ([], [Expr e]) :: Program
    in unsafeCoerce pr
}
