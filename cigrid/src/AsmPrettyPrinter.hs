module AsmPrettyPrinter where

prettyPrintAsm :: [[String]] -> String
prettyPrintAsm instr = prettyPrintAsmPrime (instr, "\tglobal\tmain\n\n\tsection\t.text\n\nmain:\n")

prettyPrintAsmPrime :: ([[String]], String) -> String
prettyPrintAsmPrime ([], s) = s
prettyPrintAsmPrime (instr : t, s) = prettyPrintAsmPrime (t, s ++ s2)
    where s2 = prettyPrintInstr instr

prettyPrintInstr :: [String] -> String
prettyPrintInstr [op, r1, r2 ] = "\t" ++ op ++ "\t" ++ r1 ++ ", " ++ r2 ++ "\n"
prettyPrintInstr [op, r1]      = "\t" ++ op ++ "\t" ++ r1 ++ "\n"
prettyPrintInstr [op]          = "\t" ++ op ++ "\n"
prettyPrintInstr []            = ""
