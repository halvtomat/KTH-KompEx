module InstructionGenerator where

import Parser

genInstr :: Program -> String
genInstr (Program [GlobalFuncDef(TyInt, "main", _ , stmt)]) = 
    "\tglobal\tmain\n\n\tsection\t.text\n\nmain:\n\tsub\trsp, " ++ show (c*8) ++ "\n" ++ 
    s ++ "\tadd\trsp, " ++ show (c*8) ++ "\n\tret"
    where (s, c) = genInstrStmt (stmt, 0) 

genInstrStmt :: (Stmt, Int) -> (String, Int)
genInstrStmt (StmtScope s, c) = genInstrStmtScope (s, "", c)
genInstrStmt (StmtReturn e, c) = ("\tmov\trax, " ++ ex ++ "\n", c)
    where ex = genInstrExpr e
genInstrStmt (StmtVarDef (t, i, e), c) = ("\tmov\t" ++ getReg i ++ ", " ++ ex ++ "\n", c+1)
    where ex = genInstrExpr e
genInstrStmt (StmtExpr e, c) = ("\t" ++ ex ++ "\n", c)
    where ex = genInstrExpr e
genInstrStmt (_, c)  = ("\tget\trekt\n", c);

genInstrStmtScope :: ([Stmt], String, Int) -> (String, Int)
genInstrStmtScope (stmt : t, s, c) = genInstrStmtScope (t, s ++ s2, c2)
    where (s2, c2) = genInstrStmt (stmt, c)
genInstrStmtScope ([], s, c) = (s, c)

genInstrExpr :: Expr-> String
genInstrExpr (ExprInt i) = show i
genInstrExpr (ExprIdent i) = getReg i

getReg :: String -> String
getReg "x" = "qword [rsp]"
getReg "y" = "qword [rsp+8]"