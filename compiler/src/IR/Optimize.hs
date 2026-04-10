module IR.Optimize where

import IR.TAC (IRProgm(..), IRClass(..), IRFunction(..), IRBlock(..),
    flattenIRProgm, pruneIRProgm, rmEBInProg)

import qualified IR.TAC as TAC


-- | Strip phi nodes after edge expansion.
stripPhiProgm :: IRProgm -> IRProgm
stripPhiProgm (IRProgm pkg classes) = IRProgm pkg (map stripPhiClass classes)


stripPhiClass :: IRClass -> IRClass
stripPhiClass (IRClass decl name fields (TAC.StaticInit (blocks, retBid)) atomTypes funs mainKind) =
    IRClass decl name fields (TAC.StaticInit (map stripPhiBlock blocks, retBid)) atomTypes (map stripPhiFunction funs) mainKind


stripPhiFunction :: IRFunction -> IRFunction
stripPhiFunction (IRFunction acc name sig atomTypes (blocks, retBid) memberType) =
    IRFunction acc name sig atomTypes (map stripPhiBlock blocks, retBid) memberType


stripPhiBlock :: IRBlock -> IRBlock
stripPhiBlock (IRBlock (bid, instrs)) = IRBlock (bid, stripPhiStmts instrs)


stripPhiStmts :: [TAC.IRInstr] -> [TAC.IRInstr]
stripPhiStmts = concatMap stripPhiStmt


stripPhiStmt :: TAC.IRInstr -> [TAC.IRInstr]
stripPhiStmt instr
    | isPhiAssign instr = []
    | otherwise = [instr]


isPhiAssign :: TAC.IRInstr -> Bool
isPhiAssign instr = case instr of
    TAC.IAssign _ (TAC.Phi _) -> True
    _ -> False


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
