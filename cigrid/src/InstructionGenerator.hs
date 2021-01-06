module InstructionGenerator where

import Parser

data Reg = Reg(String, Int)

genInstr :: Program -> [[String]]
genInstr (Program [GlobalFuncDef(TyInt, "main", _ , stmt)]) = [["sub","rsp",show (regCount rl * 8)]] ++ s
    where (s, rl) = genInstrStmt (stmt, []) 

genInstrStmt :: (Stmt, [Reg]) -> ([[String]], [Reg])
genInstrStmt (StmtScope s, rl) = genInstrStmtScope (s, [], rl)
genInstrStmt (StmtReturn e, rl) = (ex ++ [
    ["mov", "rax", getReg(retReg, rl2)],
    ["add", "rsp", show (regCount rl2 * 8)], 
    ["ret"]], rl2)
    where (ex, retReg, rl2) = genInstrExpr (e, rl)
genInstrStmt (StmtVarDef (t, i, e), rl) = (ex ++ [
    ["mov", "r10", getReg(retReg, rl3)],
    ["mov", getReg (i, rl2), "r10"]], rl3)
    where 
        (ex, retReg, rl3) = genInstrExpr (e, rl2)
        rl2 = assignReg (i, rl)
genInstrStmt (StmtExpr e, rl) = (ex, rl2)
    where (ex, _, rl2) = genInstrExpr (e, rl)
genInstrStmt (StmtVarAss (i, e), rl) = (ex ++ [
    ["mov", "r10", getReg(retReg, rl2)],
    ["mov", getReg (i, rl), "r10"]], rl2)
    where (ex, retReg, rl2) = genInstrExpr (e, rl)
genInstrStmt (_, rl)  = ([
    ["error", "error"]], rl);

genInstrStmtScope :: ([Stmt], [[String]], [Reg]) -> ([[String]], [Reg])
genInstrStmtScope (stmt : t, s, rl) = genInstrStmtScope (t, s ++ s2, rl2)
    where (s2, rl2) = genInstrStmt (stmt, rl)
genInstrStmtScope ([], s, rl) = (s, rl)

genInstrExpr :: (Expr, [Reg]) -> ([[String]], String, [Reg])
genInstrExpr (ExprInt i, rl) = ([
    ["mov", getReg(retReg, rl2), show i]], retReg, rl2)
    where (retReg, rl2) = assignTempReg rl
genInstrExpr (ExprChar c, rl) = ([
    ["mov", getReg(retReg, rl2), show c]], retReg, rl2)
    where (retReg, rl2) = assignTempReg rl
genInstrExpr (ExprIdent i, rl) = ([
    ["mov", "r10", getReg (i, rl)],
    ["mov", getReg(retReg, rl2), "r10"]], retReg, rl2)
    where (retReg, rl2) = assignTempReg rl
genInstrExpr (ExprBinOp (BinOpAdd, e1, e2), rl) = (ex1 ++ ex2 ++ [
    ["mov", "r10", getReg(e1Ret, rl2)],
    ["add", "r10", getReg(e2Ret, rl3)],
    ["mov", getReg(retReg, rl4), "r10"]], retReg, rl4)
    where   (ex1, e1Ret, rl2) = genInstrExpr (e1, rl)
            (ex2, e2Ret, rl3) = genInstrExpr (e2, rl2)
            (retReg, rl4) = assignTempReg rl3

getReg :: (String, [Reg]) -> String
getReg (s, Reg(i, c) : t)
    | s == i = "qword [rsp+" ++ show((c-1)*8) ++ "]"
    | otherwise = getReg(s, t)
getReg (_, []) = error "Undeclared Variable"

assignReg :: (String, [Reg]) -> [Reg]
assignReg (s, rl) = rl ++ [Reg(s, regCount rl + 1)]

assignTempReg :: [Reg] -> (String, [Reg])
assignTempReg rl = (i, rl ++ [Reg(i, c + 1)])
    where 
        c = regCount rl
        i = "tmp"++show(c+1)

regCount :: [Reg] -> Int
regCount [] = 0
regCount rl = regCountPrime (rl, 0)

regCountPrime :: ([Reg], Int) -> Int
regCountPrime ([], c) = c
regCountPrime ((_ : t), c) = regCountPrime (t, c+1)