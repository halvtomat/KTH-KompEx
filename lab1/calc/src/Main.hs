module Main where

import Data.Char (isSpace, isDigit)

data Token =
    TokAdd
    | TokSub
    | TokMul
    | TokDiv
    | TokNum(Int)
    | TokLPar
    | TokRPar
    | TokEOF
    deriving (Show)

data Expr =
    ExprAdd(Expr, Expr)
    | ExprSub(Expr, Expr)
    | ExprMul(Expr, Expr)
    | ExprDiv(Expr, Expr)
    | ExprNum(Int) 
    deriving (Show)

tokenize :: [Char] -> [Token]
tokenize s@(d : _) | isDigit d = 
    let (num , t) = span isDigit s
    in TokNum (read num) : tokenize t
tokenize ('+' : t) = TokAdd : tokenize t
tokenize ('-' : t) = TokSub : tokenize t
tokenize ('*' : t) = TokMul : tokenize t
tokenize ('/' : t) = TokDiv : tokenize t
tokenize ('(' : t) = TokLPar : tokenize t
tokenize (')' : t) = TokRPar : tokenize t
tokenize (x : t) | isSpace x = tokenize t
tokenize [] = [TokEOF]

parseMain :: [Token] -> Expr 
parseMain tok = expr 
    where ((TokEOF : _), expr) = parseExpr tok

parseExpr :: [Token] -> ([Token], Expr)
parseExpr tok = parseExprPrime (parseTerm tok)

parseExprPrime :: ([Token], Expr) -> ([Token], Expr)
parseExprPrime ((TokAdd : t), expr) = parseExprPrime (tok2, ExprAdd (expr, expr2))
    where (tok2, expr2) = parseTerm t
parseExprPrime ((TokSub : t), expr) = parseExprPrime (tok2, ExprSub (expr, expr2))
    where (tok2, expr2) = parseTerm t
parseExprPrime (tok, expr) = (tok, expr)

parseTerm :: [Token] -> ([Token], Expr)
parseTerm tok = parseTermPrime (parseFactor tok)

parseTermPrime :: ([Token], Expr) -> ([Token], Expr)
parseTermPrime ((TokMul : t), expr) = parseTermPrime (tok2, ExprMul (expr, expr2))
    where (tok2, expr2) = parseTerm t
parseTermPrime ((TokDiv : t), expr) = parseTermPrime (tok2, ExprDiv (expr, expr2))
    where (tok2, expr2) = parseTerm t
parseTermPrime (tok, expr) = (tok, expr)

parseFactor :: [Token] -> ([Token], Expr)
parseFactor (TokNum(v) : t) = (t, ExprNum(v))
parseFactor (TokLPar : t) = (t2, expr)
    where ((TokRPar : t2), expr) = parseExpr t



main :: IO ()
main = print $ tokenize "10+5-3"
    

