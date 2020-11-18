module PrettyPrinter where 

import Parser

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program x) = prettyPrintProgramPrime (x, "{")

prettyPrintProgramPrime :: ([Global], String) -> String
prettyPrintProgramPrime ([], s) = s ++ "\n}"
prettyPrintProgramPrime (g : t, s) = prettyPrintProgramPrime (t, s ++ "\n" ++ prettyPrintGlobal g)

prettyPrintGlobal :: Global -> String
prettyPrintGlobal (GlobalFuncDef (ty, i, args, stmt)) = 
    "GFuncDef (" ++ 
    prettyPrintType ty ++ 
    ", " ++ (show i) ++ ", " ++ 
    prettyPrintArgs args ++
    ", " ++ prettyPrintStmt stmt ++
    ")"
prettyPrintGlobal (GlobalFuncDecl (ty, i, args)) = 
    "GFuncDecl (" ++
    prettyPrintType ty ++ 
    ", " ++ (show i) ++ ", " ++ 
    prettyPrintArgs args ++
    ")"

prettyPrintArgs :: [Arg] -> String
prettyPrintArgs x = prettyPrintArgsPrime (x, "{")

prettyPrintArgsPrime :: ([Arg], String) -> String
prettyPrintArgsPrime ([], s) = s ++ "}"
prettyPrintArgsPrime ((Arg(ty, i)) : t, s) = prettyPrintArgsPrime (t, s ++ "(" ++ prettyPrintType ty ++ ", " ++ (show i) ++ ")")

prettyPrintType :: Ty -> String
prettyPrintType TyVoid = "TVoid"
prettyPrintType TyInt = "TInt"
prettyPrintType TyChar = "TChar"

prettyPrintStmt :: Stmt -> String
prettyPrintStmt (StmtExpr e) = "SExpr (" ++ prettyPrintExpr e ++ ")"
prettyPrintStmt (StmtVarDef (ty, i, e)) = 
    "SVarDef (" ++ 
    prettyPrintType ty ++ 
    ", " ++ (show i) ++ ", " ++ 
    prettyPrintExpr e ++ 
    ")"
prettyPrintStmt (StmtVarAss (i, e)) = "SVarAssign (" ++ (show i) ++ ", " ++ prettyPrintExpr e ++ ")"
prettyPrintStmt (StmtWhile (e, s)) = "SWhile (" ++ prettyPrintExpr e ++ ", " ++ prettyPrintStmt s ++ ")"
prettyPrintStmt (StmtIf (e, s)) = "SIf (" ++ prettyPrintExpr e ++ ", " ++ prettyPrintStmt s ++ ", )"
prettyPrintStmt (StmtIfElse (e, s, s2)) = 
    "SIf (" ++ 
    prettyPrintExpr e ++ 
    ", " ++ prettyPrintStmt s ++ 
    ", " ++ prettyPrintStmt s2 ++
    ")"
prettyPrintStmt (StmtBreak) = "SBreak"
prettyPrintStmt (StmtReturn e) = "SReturn (" ++ prettyPrintExpr e ++ ")"
prettyPrintStmt (StmtReturnEmpty) = "SReturn ()"
prettyPrintStmt (StmtScope s) = prettyPrintStmtScope (s, "SScope {")

prettyPrintStmtScope :: ([Stmt], String) -> String
prettyPrintStmtScope ([], s) = s ++ "}"
prettyPrintStmtScope (stmt : t, s) = prettyPrintStmtScope (t, s ++ " " ++ prettyPrintStmt stmt)

prettyPrintExpr :: Expr -> String
prettyPrintExpr (ExprIdent i) = "EVar (" ++ (show i) ++ ")"
prettyPrintExpr (ExprInt n) = "EInt (" ++ (show n) ++ ")"
prettyPrintExpr (ExprChar c) = "EChar (" ++ (show c) ++ ")"
prettyPrintExpr (ExprString s) = "EString (" ++ (show s) ++ ")"
prettyPrintExpr (ExprBinOp (op, e1, e2)) = 
    "EBinOp (" ++ 
    prettyPrintBinOp op ++ 
    ", " ++ prettyPrintExpr e1 ++
    ", " ++ prettyPrintExpr e2 ++
    ")"
prettyPrintExpr (ExprCall (i, e)) = prettyPrintExprCall (e, ("ECall (" ++ (show i) ++ ", {"))

prettyPrintExprCall :: ([Expr], String) -> String
prettyPrintExprCall ([], s) = s ++ "}"
prettyPrintExprCall (e : t, s) = prettyPrintExprCall (t, s ++ " " ++ prettyPrintExpr e)

prettyPrintBinOp :: BinOp -> String
prettyPrintBinOp BinOpAdd = "+"
prettyPrintBinOp BinOpSub = "-"
prettyPrintBinOp BinOpMul = "*"
prettyPrintBinOp BinOpDiv = "/"
prettyPrintBinOp BinOpMod = "%"
prettyPrintBinOp BinOpLess = "<"
prettyPrintBinOp BinOpGreat = ">"
prettyPrintBinOp BinOpLessEq = "<="
prettyPrintBinOp BinOpGreatEq = ">="
prettyPrintBinOp BinOpEq = "=="
prettyPrintBinOp BinOpNotEq = "!="
prettyPrintBinOp BinOpAnd = "&"
prettyPrintBinOp BinOpOr = "|"
prettyPrintBinOp BinOpAndLog = "&&"
prettyPrintBinOp BinOpOrLog = "||"