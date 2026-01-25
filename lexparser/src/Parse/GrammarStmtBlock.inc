Sep :: { () }
    : ';' { () }
    | NL  { () }
    ;

Seps :: { () }
    : {- empty -} { () }
    | Seps Sep    { () }
    ;

Seps1 :: { () }
    : Sep         { () }
    | Seps1 Sep   { () }
    ;

StmtItem :: { Statement }
    : Stmt Seps1 { $1 }
    ;

StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq     { $1 }
    ;

StmtSeq :: { [Statement] }
    : StmtItem             { [$1] }
    | StmtSeq StmtItem     { $1 ++ [$2] }
    ;

Block :: { Block }
    : '{' Seps StmtSeqOpt Seps '}' { Multiple $3 }
    ;

Stmt :: { Statement }
    : Expr  { Expr $1 }
    | Block { BlockStmt $1 }
    ;


StmtEOF   :: { Statement } : Seps Stmt   Seps EOF { $2 };
BlockEOF  :: { Block }     : Seps Block  Seps EOF { $2 };


ProgramEOF :: { Program }
    : Seps StmtSeqOpt Seps EOF { ([], $2) }
    ;
