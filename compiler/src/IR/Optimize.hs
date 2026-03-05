module IR.Optimize where

import IR.TAC (IRProgm(..), IRClass(..), IRFunction(..), IRStmt(..), IRBlock(..),
    flattenIRProgm, pruneIRProgm, rmEBInProg)

import qualified IR.TAC as TAC


-- | Strip phi nodes after edge expansion.
stripPhiProgm :: IRProgm -> IRProgm
stripPhiProgm (IRProgm pkg classes) = IRProgm pkg (map stripPhiClass classes)


stripPhiClass :: IRClass -> IRClass
stripPhiClass (IRClass decl name fields (TAC.StaticInit stmts) atomTypes funs) =
    IRClass decl name fields (TAC.StaticInit (stripPhiStmts stmts)) atomTypes (map stripPhiFunction funs)


stripPhiFunction :: IRFunction -> IRFunction
stripPhiFunction (IRFunction acc name sig atomTypes stmts) =
    IRFunction acc name sig atomTypes (stripPhiStmts stmts)


stripPhiStmts :: [IRStmt] -> [IRStmt]
stripPhiStmts = concatMap stripPhiStmt


stripPhiStmt :: IRStmt -> [IRStmt]
stripPhiStmt stmt = case stmt of
    IRInstr (TAC.IAssign _ (TAC.Phi _)) -> []
    IRInstr _ -> [stmt]
    IRBlockStmt (IRBlock (bid, stmts)) ->
        [IRBlockStmt (IRBlock (bid, stripPhiStmts stmts))]


-- | Flatten nested blocks, prune redundant gotos, then remove empty blocks.
formateIR :: IRProgm -> IRProgm
formateIR = foldr (.) id [
    rmEBInProg,
    pruneIRProgm,
    stripPhiProgm,
    optimizeProgm,
    flattenIRProgm]


optimizeProgm :: IRProgm -> IRProgm
optimizeProgm = id
