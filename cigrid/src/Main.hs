{-Daniels Cigrid Compiler, a compiler for Cigrid, a subset of C-}

module Main where

import System.Exit ( exitWith, ExitCode(ExitFailure, ExitSuccess))
import System.IO
import System.Environment
import Options.Applicative
import Data.Semigroup((<>))
import Control.Exception ( catch, PatternMatchFail(PatternMatchFail))
import Tokenizer
import Parser
import PrettyPrinter

data Args = Args
    { prettyprint :: Bool 
    , filename :: String }

args :: Parser Args
args = Args
    <$> switch
         ( long "pretty-print"
        <> short 'p'
        <> help "Use to print pretty" )
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
compile (Args True f) = do
    file <- openFile f ReadMode
    input <- hGetContents file
    let tok = tokenize input
    let program = parseMain tok
    let prettyPrint = prettyPrintProgram program
    putStrLn prettyPrint
    exitWith (ExitSuccess)

compile (Args False f) = do
    file <- openFile f ReadMode
    input <- hGetContents file
    let tok = tokenize input
    let program = parseMain tok
    exitWith (ExitSuccess)

handler :: PatternMatchFail -> IO ()
handler (PatternMatchFail _) = exitWith (ExitFailure 1)