
module Generator.X86Assembly where

import Foreign (touchForeignPtr)
import Text.Printf

data Register = RDI | RSI | RBP | RAX | R8 | R9

instance Show Register where
    show RDI = "rdi"
    show RSI = "rsi"
    show RBP = "rbp"
    show RAX = "rax"
    show R8 = "r8"
    show R9 = "r9"

data X86Data = X86Data {variableName :: String, value :: String, end :: Integer} deriving Show

data X86Instruction = MOV Register String | CALL String |
        Extern String | Global String | Default String | StringPair String String
        | ADD Register Register | SUB Register Register | IMUL Register Register
        | PUSH Register | POP Register | TextSection | DataSection | RET | StartMain


instance Show X86Instruction where
    show (MOV reg value) = printf "mov %s, %s" (show reg) value
    show (CALL value) = printf "call %s" value
    show (Extern value) = printf "extern %s" value
    show (Default value) = printf "default %s" value
    show (Global value) = printf "global %s" value
    show (StringPair val1 val2) = printf "%s %s" val1 val2
    show (ADD reg1 reg2) = printf "add %s, %s" (show reg1) (show reg2)
    show (SUB reg1 reg2) = printf "sub %s, %s" (show reg1) (show reg2)
    show (IMUL reg1 reg2) =  printf "imul %s, %s" (show reg1) (show reg2)
    show (PUSH reg) = printf "push %s" (show reg)
    show (POP reg) = printf "pop %s" (show reg)
    show TextSection = "section .text"
    show DataSection = "section .data"
    show RET = "ret"
    show StartMain = "_main:"

data X86Assembly = X86Assembly {codeSection :: [X86Instruction], dataSection :: [X86Data] } deriving Show

mergeAsm :: X86Assembly -> X86Assembly -> X86Assembly
mergeAsm X86Assembly {codeSection = codeSectionOld, dataSection = dataSectionOld}
    X86Assembly {codeSection = codeSectionNew, dataSection = dataSectionNew} = X86Assembly
        {codeSection = codeSectionOld ++ codeSectionNew, dataSection = dataSectionOld ++ dataSectionNew }

getEmptyX86Asm :: X86Assembly
getEmptyX86Asm = X86Assembly {codeSection = [], dataSection = []}

addCodeSection :: X86Assembly -> [X86Instruction] -> X86Assembly
addCodeSection existing codeSection = mergeAsm existing (X86Assembly
                                                {dataSection = [], codeSection = codeSection})
