{-Daniels Cigrid Compiler, a compiler for Cigrid, a subset of C-}

module Main where

import System.Exit ( exitWith, ExitCode(ExitFailure, ExitSuccess))
import System.IO
import Options.Applicative
import Control.Exception ( catch, PatternMatchFail(PatternMatchFail))
import Tokenizer
import Parser
import AstPrettyPrinter
import AsmPrettyPrinter
import InstructionGenerator

data Args = Args
    { prettyprint :: Bool 
    , asm :: Bool
    , filename :: String }

args :: Parser Args
args = Args
    <$> switch
         ( long "pretty-print"
        <> short 'p'
        <> help "Pretty-print the AST" )
    <*> switch
         ( long "asm"
        <> short 'a'
        <> help "Print the generated x86 instructions" )
    <*> argument str
         ( metavar "FILE"
        <> help "Name of file to compile" )

opts :: ParserInfo Args
opts = info (args <**> helper)
    ( fullDesc
    <> progDesc "Compile the Cigrid language, a subset of C"
    <> header "DCC - Daniels Cigrid Compiler")


main :: IO()
main = catch (compile =<< execParser opts) handler

compile :: Args -> IO()
compile (Args True False f) = do
    file <- openFile f ReadMode
    input <- hGetContents file
    let tok = tokenize input
    let program = parseMain tok
    let prettyPrint = prettyPrintProgram program
    putStrLn prettyPrint
    exitWith (ExitSuccess)
compile (Args False False f) = do
    file <- openFile f ReadMode
    input <- hGetContents file
    let tok = tokenize input
    let program = parseMain tok
    let _ = genInstr program
    exitWith (ExitSuccess)
compile (Args False True f) = do
    file <- openFile f ReadMode
    input <- hGetContents file
    let tok = tokenize input
    let program = parseMain tok
    let instr = genInstr program
    let prettyPrint = prettyPrintAsm instr
    putStrLn prettyPrint
    exitWith (ExitSuccess)
compile (Args True True _) = do
    putStrLn "Invalid options"
    exitWith(ExitSuccess)


handler :: PatternMatchFail -> IO ()
handler (PatternMatchFail _) = exitWith (ExitFailure 1)