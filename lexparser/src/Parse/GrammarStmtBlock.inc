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


ElseBlock :: { Maybe Block }
    : {- empty -}                   { Nothing }
    | KW_ELSE ';'                   { Nothing }
    | KW_ELSE ':' Stmt              { Just $ stmtToBlock $3 }
    ;

IfStmt :: { Statement }
    --: KW_IF Expr ';' ElseBlock          { If (Unary BitNot $2 (Lex.Symbol Lex.BitNot defaultPosition)) $4 Nothing }
    : KW_IF Expr ':' Stmt ElseBlock       { If $2 (Just $ stmtToBlock $4) $5 }
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
    

StmtEOF :: { Statement } :      Stmt EOF { $1 };

BlockEOF :: { Block } :         Block EOF { fromMaybe (Multiple []) $1 };

ProgramEOF :: { Program } :     StmtSeqOpt EOF { ([], $1) };
