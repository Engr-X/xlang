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

IfStmt :: { Statement }
    : KW_IF Expr ';'                { If $2 Nothing }
    | KW_IF Expr ':' Stmt           { If $2 (Just $ stmtToBlock $4) }
    ;

ElseStmt :: { Statement }
    : KW_ELSE ';'                   { Else Nothing }
    | KW_ELSE ':' Stmt              { Else (Just $ stmtToBlock $3) }
    ;

-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';'                 { While $2 Nothing }
    | KW_WHILE Expr ':' Stmt            { While $2 (Just $ stmtToBlock $4) }
    ;

-- assembled statement
Stmt :: { Statement }
    : Expr Sep      { Expr $1 }
    | Block         { BlockStmt $ fromMaybe (Multiple []) $1 }
    | IfStmt        { $1 }
    | ElseStmt      { $1 }
    | WhileStmt     { $1 }
    ;

-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}' Sep    { Just $ Multiple $2 }
    | '{' Sep '}' Sep           { Nothing }
    ;
    

StmtEOF :: { Statement } :      Stmt EOF { $1 };

BlockEOF :: { Block } :         Block EOF { fromMaybe (Multiple []) $1 };

ProgramEOF :: { Program } :     StmtSeqOpt EOF { ([], $1) };
