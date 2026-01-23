Term :: { () }
    : ';' { () }
    | NL  { () }
    ;

OptTerms :: { () }
    : {- empty -}   { () }
    | OptTerms Term { () }
    ;

Terms1 :: { () }
    : Term OptTerms { () }
    ;

Block :: { Block }
    : '{' OptTerms StmtSeqOpt OptTerms '}' { Multiple $3 }
    ;

StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq     { $1 }
    ;

StmtTerm :: { Statement }
    : Stmt Terms1 { $1 }
    ;

StmtSeq :: { [Statement] }
    : StmtTerm          { [$1] }
    | StmtTerm StmtSeq  { $1 : $2 }
    ;

Stmt :: { Statement }
    : Expr  { Expr $1 }
    | Block { BlockStmt $1 }
    ;
