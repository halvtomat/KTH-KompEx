module Tokenizer where

import Data.Char (isSpace, isDigit)

data Token =
    TokIntConst (Int)--CONSTANTs
    | TokCharConst (Char)
    | TokEscapeChar (Char)
    | TokStringConst (String)
    | TokLPar -- SEPARATORs
    | TokRPar
    | TokLCurl
    | TokRCurl
    | TokLSqr
    | TokRSqr
    | TokSet -- '='
    | TokComma
    | TokEOL
    | TokEOF -- EOF
    | TokIdent (String) -- IDENTIFIERs
    | TokAdd -- OPERATORs
    | TokSub
    | TokMul
    | TokDiv
    | TokNot
    | TokMod
    | TokEqual -- '=='
    | TokNotEqual
    | TokLessThan
    | TokGreatThan
    | TokGreatOrEqual
    | TokLessOrEqual
    | TokAnd
    | TokOr 
    | TokAndLog
    | TokOrLog
    | TokBreak -- KEYWORDs
    | TokExtern
    | TokNew
    | TokWhile
    | TokChar
    | TokFor
    | TokReturn
    | TokDelete
    | TokIf
    | TokStruct
    | TokElse
    | TokInt
    | TokVoid
    | TokERROR
    deriving (Show)

tokenize :: [Char] -> [Token]
tokenize [] = [TokEOF]
tokenize (x : t) | isSpace x = tokenize t -- Ignore whitespace
tokenize ('#' : t) = tokenize (commentLine t) -- Ignore #include 
tokenize ('/':'/' : t) = tokenize (commentLine t) -- Single line comment
tokenize ('/':'*' : t) = tokenize (commentSeg t) -- Multi line comment
tokenize ('\'':'\\' : t) = tokenizeEscape t -- Escape Character Literal
tokenize ('\'':x:'\'' : t) = TokCharConst (x) : tokenize t -- Character literal
tokenize s@(x : _) | isDigit x = TokIntConst (read num) : tokenize t -- Integer literal
    where (num , t) = span isDigit s
tokenize ('-':s@(x : _)) | isDigit x = TokIntConst (read num * (-1)) : tokenize t -- Negative Integer literal
    where (num , t) = span isDigit s    
tokenize ('\"' : t) = tokenizeStringConst t -- String literal
tokenize (x : t) | elem x "benwcfrdisv" = tokenizeKeyword (x : t) -- Keyword
tokenize (x : t) | elem x "+-*/=!<>|&%" = tokenizeOp (x : t) -- Operator
tokenize (x : t) | elem x "(){}[]=;," = tokenizeSeparator (x : t) -- Separator
tokenize s@(x : _) | elem x (['A'..'Z'] ++ ['a'..'z'] ++ ['_']) = tokenizeIdentifier s
tokenize _ = [TokERROR]

tokenizeEscape :: [Char] -> [Token]
tokenizeEscape ('\\':'\'' : t) = TokEscapeChar ('\\') : tokenize t
tokenizeEscape ('n':'\'' : t) = TokEscapeChar ('\n') : tokenize t
tokenizeEscape ('t':'\'' : t) = TokEscapeChar ('\t') : tokenize t
tokenizeEscape ('\'':'\'' : t) = TokEscapeChar ('\'') : tokenize t
tokenizeEscape ('\"':'\'' : t) = TokEscapeChar ('\"') : tokenize t
tokenizeEscape _ = [TokERROR]

tokenizeSeparator :: [Char] -> [Token]
tokenizeSeparator ('(' : t) = TokLPar : tokenize t
tokenizeSeparator (')' : t) = TokRPar : tokenize t
tokenizeSeparator ('{' : t) = TokLCurl : tokenize t
tokenizeSeparator ('}' : t) = TokRCurl : tokenize t
tokenizeSeparator ('[' : t) = TokLSqr : tokenize t
tokenizeSeparator (']' : t) = TokRSqr : tokenize t
tokenizeSeparator ('=' : t) = TokSet : tokenize t
tokenizeSeparator (',' : t) = TokComma : tokenize t
tokenizeSeparator (';' : t) = TokEOL : tokenize t
tokenizeSeparator x = tokenize x

tokenizeKeyword :: [Char] -> [Token]
tokenizeKeyword ('b':'r':'e':'a':'k' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokBreak : tokenize t
tokenizeKeyword ('e':'x':'t':'e':'r':'n' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokExtern : tokenize t
tokenizeKeyword ('n':'e':'w' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokNew : tokenize t
tokenizeKeyword ('w':'h':'i':'l':'e' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokWhile : tokenize t
tokenizeKeyword ('c':'h':'a':'r' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokChar : tokenize t 
tokenizeKeyword ('f':'o':'r' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokFor : tokenize t
tokenizeKeyword ('r':'e':'t':'u':'r':'n' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokReturn : tokenize t
tokenizeKeyword ('d':'e':'l':'e':'t':'e' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokDelete : tokenize t
tokenizeKeyword ('i':'f' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokIf : tokenize t
tokenizeKeyword ('s':'t':'r':'u':'c':'t' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokStruct : tokenize t
tokenizeKeyword ('e':'l':'s':'e' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokElse : tokenize t
tokenizeKeyword ('i':'n':'t' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokInt : tokenize t
tokenizeKeyword ('v':'o':'i':'d' : t@(x : _)) | elem x "(){}[]=;," || isSpace x = TokVoid : tokenize t
tokenizeKeyword (x) = tokenizeIdentifier x

tokenizeIdentifier :: [Char] -> [Token]
tokenizeIdentifier x = TokIdent (s) : tokenizeSeparator t
    where (s, t) = getIdentifier ([], x)

getIdentifier :: ([Char], [Char]) -> (String, [Char])
getIdentifier (s, (x : t)) | elem x (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_']) = getIdentifier (s ++ [x], t)
getIdentifier (s, t) = (s, t)

tokenizeStringConst :: [Char] -> [Token]
tokenizeStringConst x = TokStringConst (s) : tokenize t
    where (s, t) = getString ([], x)

getString :: ([Char], [Char]) -> (String, [Char])
getString (s, ('\"' : t)) = (s, t)
getString (s, (x : t)) = getString (s ++ [x], t)
getString (s, []) = (s, [])

tokenizeOp :: [Char] -> [Token]
tokenizeOp ('<':'=' : t) = TokLessOrEqual : tokenize t
tokenizeOp ('>':'=' : t) = TokGreatOrEqual : tokenize t
tokenizeOp ('=':'=' : t) = TokEqual : tokenize t
tokenizeOp ('!':'=' : t) = TokNotEqual : tokenize t
tokenizeOp ('|':'|' : t) = TokOrLog : tokenize t
tokenizeOp ('&':'&' : t) = TokAndLog : tokenize t
tokenizeOp ('|' : t) = TokOr : tokenize t
tokenizeOp ('&' : t) = TokAnd : tokenize t

tokenizeOp ('+' : t) = TokAdd : tokenize t
tokenizeOp ('-' : t) = TokSub : tokenize t
tokenizeOp ('*' : t) = TokMul : tokenize t
tokenizeOp ('/' : t) = TokDiv : tokenize t
tokenizeOp ('%' : t) = TokMod : tokenize t
tokenizeOp ('<' : t) = TokLessThan : tokenize t
tokenizeOp ('>' : t) = TokGreatThan : tokenize t
tokenizeOp ('!' : t) = TokNot : tokenize t
tokenizeOp x@('=' : _) = tokenizeSeparator x
tokenizeOp x = tokenize x

commentLine :: [Char] -> [Char]
commentLine [] = []
commentLine ('\n' : t) = t
commentLine (_ : t) = commentLine t

commentSeg :: [Char] -> [Char]
commentSeg [] = []
commentSeg ('*':'/' : t) = t
commentSeg (_ : t) = commentSeg t