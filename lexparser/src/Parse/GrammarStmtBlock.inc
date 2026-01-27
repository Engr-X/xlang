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
    : KW_IF Expr ';' ElseBlock          { If $2 Nothing $4 }
    | KW_IF Expr ':' Stmt ElseBlock     { If $2 (Just $ stmtToBlock $4) $5 }
    ;

ElseBlock :: { Maybe Block }
    : KW_ELSE ';'                       { Nothing }
    | KW_ELSE ':' Stmt                  { Just $ stmtToBlock $3 }
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
    | WhileStmt     { $1 }
    ;

-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}' Sep    { Just $ Multiple $2 }
    | '{' Sep '}' Sep           { Nothing }
    ;
