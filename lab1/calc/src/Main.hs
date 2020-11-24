module Main where

import Data.Char (isSpace, isDigit)
import System.IO ( isEOF )
import System.Exit ( exitWith, ExitCode(ExitFailure, ExitSuccess) )
import Control.Monad (forever, when)
import Control.Exception ( catch, ArithException(DivideByZero), SomeException, PatternMatchFail(PatternMatchFail))

data Token =
    TokAdd
    | TokSub
    | TokMul
    | TokDiv
    | TokNum Int
    | TokLPar
    | TokRPar
    | TokEOF
    | TokERROR
    deriving (Show)

data Expr =
    ExprAdd(Expr, Expr)
    | ExprSub(Expr, Expr)
    | ExprMul(Expr, Expr)
    | ExprDiv(Expr, Expr)
    | ExprNum Int
    deriving (Show)

tokenize :: [Char] -> [Token] 
tokenize [] = [TokEOF]
tokenize s@(d : _) | isDigit d = 
    let (num , t) = span isDigit s
    in TokNum(read num) : tokenize t
tokenize ('+' : t) = TokAdd : tokenize t
tokenize ('-' : t) = TokSub : tokenize t
tokenize ('*' : t) = TokMul : tokenize t
tokenize ('/' : t) = TokDiv : tokenize t
tokenize ('(' : t) = TokLPar : tokenize t
tokenize (')' : t) = TokRPar : tokenize t
tokenize (x : t) | isSpace x = tokenize t
tokenize (_ : t) = TokERROR : tokenize t

parseMain :: [Token] -> Expr 
parseMain tok = expr 
    where (TokEOF : _, expr) = parseExpr tok

parseExpr :: [Token] -> ([Token], Expr)
parseExpr tok = parseExprPrime (parseTerm tok)

parseExprPrime :: ([Token], Expr) -> ([Token], Expr)
parseExprPrime (TokAdd : t, expr) = parseExprPrime (tok2, ExprAdd (expr, expr2))
    where (tok2, expr2) = parseTerm t
parseExprPrime (TokSub : t, expr) = parseExprPrime (tok2, ExprSub (expr, expr2))
    where (tok2, expr2) = parseTerm t
parseExprPrime (tok, expr) = (tok, expr)

parseTerm :: [Token] -> ([Token], Expr)
parseTerm tok = parseTermPrime (parseFactor tok)

parseTermPrime :: ([Token], Expr) -> ([Token], Expr)
parseTermPrime (TokMul : t, expr) = parseTermPrime (tok2, ExprMul (expr, expr2))
    where (tok2, expr2) = parseFactor t
parseTermPrime (TokDiv : t, expr) = parseTermPrime (tok2, ExprDiv (expr, expr2))
    where (tok2, expr2) = parseFactor t
parseTermPrime (tok, expr) = (tok, expr)

parseFactor :: [Token] -> ([Token], Expr)
parseFactor (TokNum v : t) = (t, ExprNum v)
parseFactor (TokLPar : t) = (t2, expr)
    where (TokRPar : t2, expr) = parseExpr t

evalExpr :: Expr -> Int
evalExpr (ExprNum num) = num
evalExpr (ExprAdd(expr1, expr2)) = evalExpr expr1 + evalExpr expr2
evalExpr (ExprSub(expr1, expr2)) = evalExpr expr1 - evalExpr expr2
evalExpr (ExprMul(expr1, expr2)) = evalExpr expr1 * evalExpr expr2
evalExpr (ExprDiv(expr1, expr2)) = evalExpr expr1 `div` evalExpr expr2

prettyPrint :: Expr -> String
prettyPrint (ExprNum num) = show num
prettyPrint (ExprAdd(expr1, expr2)) = "(" ++ prettyPrint expr1 ++ " + " ++ prettyPrint expr2 ++ ")"
prettyPrint (ExprSub(expr1, expr2)) = "(" ++ prettyPrint expr1 ++ " - " ++ prettyPrint expr2 ++ ")"
prettyPrint (ExprMul(expr1, expr2)) = "(" ++ prettyPrint expr1 ++ " * " ++ prettyPrint expr2 ++ ")"
prettyPrint (ExprDiv(expr1, expr2)) = "(" ++ prettyPrint expr1 ++ " / " ++ prettyPrint expr2 ++ ")" 

skip :: [Token] -> Bool
skip (TokEOF : _) = True
skip (TokERROR : TokERROR : _) = True
skip _ = False

main :: IO()
main = catch runtimeLoop patternMatchHandler

runtimeLoop :: IO()
runtimeLoop = forever $ do
    done <- isEOF
    when done $ exitWith (ExitSuccess) 
    input <- getLine
    let tok = tokenize input
    let actuallyEOF = skip tok
    when actuallyEOF $ exitWith (ExitSuccess)
    let expr = parseMain tok
    putStrLn (prettyPrint expr)
    let ans = evalExpr expr
    catch (putStrLn ("= " ++ show ans)) divZeroHandler

patternMatchHandler :: PatternMatchFail -> IO ()
patternMatchHandler (PatternMatchFail _) = exitWith (ExitFailure 1)

divZeroHandler :: ArithException -> IO()
divZeroHandler DivideByZero = exitWith (ExitFailure 2)