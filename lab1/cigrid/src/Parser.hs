module Parser where

import Tokenizer

data BinOp =
    BinOpAdd
    | BinOpSub
    | BinOpMul
    | BinOpDiv
    | BinOpMod
    | BinOpLess
    | BinOpGreat
    | BinOpLessEq
    | BinOpGreatEq
    | BinOpEq
    | BinOpNotEq
    | BinOpAnd
    | BinOpOr
    | BinOpAndLog
    | BinOpOrLog
    deriving (Show)

data Ty =
    TyVoid
    | TyInt
    | TyChar
    deriving (Show)

data Arg =
    Arg (Ty, String)
    deriving (Show)

data Expr = 
    ExprIdent String
    | ExprInt Int
    | ExprChar Char
    | ExprString String
    | ExprBinOp(BinOp, Expr, Expr)
    | ExprCall(String, [Expr])
    deriving (Show)

data Stmt = 
    StmtExpr Expr
    | StmtVarDef(Ty, String, Expr)
    | StmtVarAss(String, Expr)
    | StmtWhile(Expr, Stmt)
    | StmtIf(Expr, Stmt)
    | StmtIfElse(Expr, Stmt, Stmt)
    | StmtBreak
    | StmtReturn(Expr) 
    | StmtReturnEmpty 
    | StmtScope([Stmt])
    deriving (Show)

data Global =
    GlobalFuncDef(Ty, String, [Arg], Stmt)
    | GlobalFuncDecl(Ty, String, [Arg])
    deriving (Show)

data Program =
    Program([Global])
    deriving (Show)

parseMain :: [Token] -> Program
parseMain t = Program p
    where ([TokEOF], p) = parseProgram (t, []) 

parseProgram :: ([Token], [Global]) -> ([Token], [Global])
parseProgram (TokEOF : _, p) = ([TokEOF], p)
parseProgram (t, p) = parseProgram (t2, p ++ [g])
    where (t2, g) = parseGlobal t

parseGlobal :: [Token] -> ([Token], Global)
parseGlobal (ty : TokIdent i : TokLPar : t) = parseGlobalPrime (t2, ty2, i, args)
    where 
        (t2, args) = parseArgs (t, [])
        ty2 = parseType ty

parseGlobalPrime :: ([Token], Ty, String, [Arg]) -> ([Token], Global)
parseGlobalPrime (TokEOL : t, ty, i, args) = (t, GlobalFuncDecl(ty, i, args))
parseGlobalPrime (t, ty, i, args) = (t2, GlobalFuncDef(ty, i, args, stmt))
    where (t2, stmt) = parseStatement t

parseStatement :: [Token] -> ([Token], Stmt)
parseStatement (TokLCurl : t) = (t2, StmtScope stmt)
    where (t2, stmt) = parseStatementScope (t, [])
parseStatement (TokBreak : TokEOL : t) = (t, StmtBreak)
parseStatement (TokReturn : TokEOL: t) = (t, StmtReturnEmpty)
parseStatement (TokReturn : t) = (t2, StmtReturn expr)
    where (TokEOL : t2, expr) = parseExpr t
parseStatement (TokIf : TokLPar : t) = parseStatementIf (t3, expr, stmt)
    where 
        (TokRPar : t2, expr) = parseExpr t
        (t3, stmt) = parseStatement t2
parseStatement (TokWhile : TokLPar : t) = (t3, StmtWhile(expr, stmt))
    where 
        (TokRPar : t2, expr) = parseExpr t
        (t3, stmt) = parseStatement t2
parseStatement (TokIdent i : TokAdd : TokAdd : TokEOL : t) = (t, StmtVarAss(i, ExprBinOp(BinOpAdd, ExprIdent i, ExprInt 1))) -- ++ Operator
parseStatement (TokIdent i : TokSub : TokSub : TokEOL : t) = (t, StmtVarAss(i, ExprBinOp(BinOpSub, ExprIdent i, ExprInt 1))) -- -- Operator
parseStatement (TokIdent i : TokSet : t) = (t2, StmtVarAss(i, expr))
    where (TokEOL : t2, expr) = parseExpr t
parseStatement (ty : TokIdent i : TokSet : t) = (t2, StmtVarDef(ty2, i, expr))
    where 
        (TokEOL : t2, expr) = parseExpr t
        ty2 = parseType ty
parseStatement t = (t2, StmtExpr expr)
    where (TokEOL : t2, expr) = parseExpr t

parseStatementIf :: ([Token], Expr, Stmt) -> ([Token], Stmt)
parseStatementIf (TokElse : t, expr, stmt) = (t2, StmtIfElse (expr, stmt, stmt2))
    where (t2, stmt2) = parseStatement t
parseStatementIf (t, expr, stmt) = (t, StmtIf(expr, stmt))

parseStatementScope :: ([Token], [Stmt]) -> ([Token], [Stmt])
parseStatementScope (TokRCurl : t, stmt) = (t, stmt)
parseStatementScope (t, scope) = parseStatementScope (t2, scope ++ [stmt])
    where (t2, stmt) = parseStatement t

parseExpr :: [Token] -> ([Token], Expr)
parseExpr t@(TokIdent _ : TokLPar : _) = parseExprCall t
parseExpr tok = parseExprlvl1 tok

parseExprlvl1 :: [Token] -> ([Token], Expr)
parseExprlvl1 tok = parseExprlvl1prime (parseExprlvl2 tok)

parseExprlvl1prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl1prime (TokOrLog : t, expr) = parseExprlvl1prime (tok2, ExprBinOp (BinOpOrLog, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl1prime (tok, expr) = (tok, expr)

parseExprlvl2 :: [Token] -> ([Token], Expr)
parseExprlvl2 tok = parseExprlvl2prime (parseExprlvl3 tok)

parseExprlvl2prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl2prime (TokAndLog : t, expr) = parseExprlvl2prime (tok2, ExprBinOp (BinOpAndLog, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl2prime (tok, expr) = (tok, expr)

parseExprlvl3 :: [Token] -> ([Token], Expr)
parseExprlvl3 tok = parseExprlvl3prime (parseExprlvl4 tok)

parseExprlvl3prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl3prime (TokOr : t, expr) = parseExprlvl3prime (tok2, ExprBinOp (BinOpOr, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl3prime (tok, expr) = (tok, expr)

parseExprlvl4 :: [Token] -> ([Token], Expr)
parseExprlvl4 tok = parseExprlvl4prime (parseExprlvl5 tok)

parseExprlvl4prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl4prime (TokAnd : t, expr) = parseExprlvl4prime (tok2, ExprBinOp (BinOpAnd, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl4prime (tok, expr) = (tok, expr)

parseExprlvl5 :: [Token] -> ([Token], Expr)
parseExprlvl5 tok = parseExprlvl5prime (parseExprlvl6 tok)

parseExprlvl5prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl5prime (TokEqual : t, expr) = parseExprlvl5prime (tok2, ExprBinOp (BinOpEq, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl5prime (TokNotEqual : t, expr) = parseExprlvl5prime (tok2, ExprBinOp (BinOpNotEq, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl5prime (tok, expr) = (tok, expr)

parseExprlvl6 :: [Token] -> ([Token], Expr)
parseExprlvl6 tok = parseExprlvl6prime (parseExprlvl7 tok)

parseExprlvl6prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl6prime (TokLessThan : t, expr) = parseExprlvl6prime (tok2, ExprBinOp (BinOpLess, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl6prime (TokLessOrEqual : t, expr) = parseExprlvl6prime (tok2, ExprBinOp (BinOpLessEq, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl6prime (TokGreatThan : t, expr) = parseExprlvl6prime (tok2, ExprBinOp (BinOpGreat, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl6prime (TokGreatOrEqual : t, expr) = parseExprlvl6prime (tok2, ExprBinOp (BinOpGreatEq, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl6prime (tok, expr) = (tok, expr)

parseExprlvl7 :: [Token] -> ([Token], Expr)
parseExprlvl7 tok = parseExprlvl7prime (parseExprlvl8 tok)

parseExprlvl7prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl7prime (TokAdd : t, expr) = parseExprlvl7prime (tok2, ExprBinOp (BinOpAdd, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl7prime (TokSub : t, expr) = parseExprlvl7prime (tok2, ExprBinOp (BinOpSub, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl7prime (tok, expr) = (tok, expr)

parseExprlvl8 :: [Token] -> ([Token], Expr)
parseExprlvl8 tok = parseExprlvl8prime (parseExprlvl9 tok)

parseExprlvl8prime :: ([Token], Expr) -> ([Token], Expr)
parseExprlvl8prime (TokMul : t, expr) = parseExprlvl8prime (tok2, ExprBinOp (BinOpMul, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl8prime (TokDiv : t, expr) = parseExprlvl8prime (tok2, ExprBinOp (BinOpDiv, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl8prime (TokMod : t, expr) = parseExprlvl8prime (tok2, ExprBinOp (BinOpMod, expr, expr2))
    where (tok2, expr2) = parseExprlvl1 t
parseExprlvl8prime (tok, expr) = (tok, expr)

parseExprlvl9 :: [Token] -> ([Token], Expr)
parseExprlvl9 (TokIdent i : t) = (t, ExprIdent i)
parseExprlvl9 (TokCharConst c : t) = (t, ExprChar c)
parseExprlvl9 (TokEscapeChar c : t) = (t, ExprChar c)
parseExprlvl9 (TokIntConst i : t) = (t, ExprInt i) 
parseExprlvl9 (TokStringConst s : t) = (t, ExprString s)
parseExprlvl9 (TokLPar : t) = (t2, expr)
    where (TokRPar : t2, expr) = parseExpr t

parseExprCall :: [Token] -> ([Token], Expr)
parseExprCall (TokIdent i : TokLPar : t) = (t2, ExprCall(i, params))
    where (t2, params) = parseExprCallPrime (t, [])

parseExprCallPrime :: ([Token], [Expr]) -> ([Token], [Expr])
parseExprCallPrime (TokRPar : t, params) = (t, params)
parseExprCallPrime (TokComma: t, params) = parseExprCallPrime (t2, params ++ [expr])
    where (t2, expr) = parseExpr t
parseExprCallPrime (t, params) = parseExprCallPrime (t2, params ++ [expr])
    where (t2, expr) = parseExpr t

parseArgs :: ([Token], [Arg]) -> ([Token], [Arg])
parseArgs (TokRPar : t, args) = (t, args)
parseArgs (TokComma : ty : TokIdent i : t, args) = parseArgs (t, args ++ [Arg(ty2, i)])
    where ty2 = parseType ty
parseArgs (ty : TokIdent i : t, args) = parseArgs (t, args ++ [Arg(ty2, i)])
    where ty2 = parseType ty

parseType :: Token -> Ty
parseType TokVoid = TyVoid
parseType TokInt = TyInt
parseType TokChar = TyChar