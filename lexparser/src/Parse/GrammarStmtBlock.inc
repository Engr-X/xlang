-- separators
Term :: { () }
    : ';' { () }
    | NL  { () }

Terms0 :: { () }          -- 0 or more separators
    : {- empty -}         { () }
    | Terms0 Term         { () }

Terms1 :: { () }          -- 1 or more separators
    : Term Terms0         { () }

-- statements
Stmt :: { Statement }
    : Expr                { Expr $1 }
    | Block               { BlockStmt $1 }
    ;

-- block
Block :: { Block }
    : '{' Terms0 StmtSeqOpt '}'   { Multiple $3 }

StmtSeqOpt :: { [Statement] }
    : {- empty -}                 { [] }
    | StmtSeq                     { $1 }

StmtSeq :: { [Statement] }
    : Stmt StmtSeqTail            { $1 : $2 }

StmtSeqTail :: { [Statement] }
    : {- empty -}                 { [] }
    | Terms1 Stmt StmtSeqTail     { $2 : $3 }
