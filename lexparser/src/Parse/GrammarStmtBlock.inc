Sep :: { () }
    : ';' { () }
    | NL  { () }
    ;

StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq     { $1 }
    ;

StmtSeq :: { [Statement] }
    : Stmt             { [$1] }
    | StmtSeq Stmt     { $1 ++ [$2] }
    ;

-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';'         { While $2 Nothing }
    | KW_WHILE Expr NL Stmt     { While $2 (Just $ stmtToBlock $4) }
    | KW_WHILE Expr Block       { While $2 (Just $3) }
    ;

-- assembled statement
Stmt :: { Statement }
    : Expr Sep      { Expr $1 }
    | Block         { BlockStmt $1 }
    | WhileStmt     { $1 }
    ;


-- block
Block :: { Block }
    : '{' StmtSeqOpt '}' Sep    { Multiple $2 }
    ;
    

StmtEOF :: { Statement } :      Stmt EOF { $1 };

BlockEOF :: { Block } :         Block EOF { $1 };

ProgramEOF :: { Program } :     StmtSeqOpt EOF { ([], $1) };
