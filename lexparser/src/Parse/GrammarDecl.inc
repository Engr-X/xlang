Decl :: { Declaration }
    : KW_PACKAGE QName Sep     { let (ss, ts) = $2 in Package (reverse ss) (reverse ts) }
    | KW_IMPORT QName Sep     { let (ss, ts) = $2 in Import (reverse ss) (reverse ts) }
    ;


DeclSeq :: { [Declaration] }
    : Decl             { [$1] }
    | DeclSeq Decl     { $1 ++ [$2] }
    ;


DeclSeqOpt :: { [Declaration] }
    : {- empty -}     { [] }
    | DeclSeq         { $1 }
    ;
