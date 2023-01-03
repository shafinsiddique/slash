
module Generator.X86Assembly where

import Foreign (touchForeignPtr)
import Text.Printf
import Generator.SymbolTable
import Data.List

data DoubleRegister = XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7

instance Show DoubleRegister where
    show XMM0 = "xmm0"
    show XMM1 = "xmm1"
    show XMM2 = "xmm2"
    show XMM3 = "xmm3"
    show XMM4 = "xmm4"
    show XMM5 = "xmm5"
    show XMM6 = "xmm6"
    show XMM7 = "xmm7"
 
data Register = RDI | RSI | RBP | RAX | R8 | R9 | RSP | R10 | R11 | RCX | RDX | 
                    DoubleReg DoubleRegister

instance Show Register where
    show RDI = "rdi"
    show RSI = "rsi"
    show RBP = "rbp"
    show RAX = "rax"
    show R8 = "r8"
    show R9 = "r9"
    show RSP = "rsp"
    show R10 = "r10"
    show R11 = "r11"
    show RCX = "rcx"
    show RDX = "rdx"
    show (DoubleReg reg)  = show reg


data X86Instruction = MOV Register String | CALL String |
        Extern String | Global String | Default String | StringPair String String
        | ADD Register Register | SUB Register Register | IMUL Register Register
        | PUSH Register | POP Register | TextSection | DataSection | RET | StartMain
        | MOVR Register Register
        | X86Data String String Integer
        | MOVToMem Register Int Register
        | SUBI Register Integer
        | ADDI Register Integer
        | MOVFromMem Register Int Register
        | MOVPFromMem Register Int Register
        | CMPRI Register Integer 
        | JZ String 
        | JMP String 
        | Section String
        | MOVI Register Integer
        | DIV Register
        | MOVUPS Register String Int
        | DoublesArray String [Double]
        | ADDSD Register Register | SUBSD Register Register | MULSD Register Register 
        | DIVSD Register Register
        | MOVFloatToMem Register Int Register
        | MOVFloatFromMem Register Int Register 
        | MOVSD Register Register
        | MOVPToMem Register Int Register
        | PUSHDouble Register
        | POPDouble Register


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
    show (MOVR reg1 reg2) = printf "mov %s, %s" (show reg1) (show reg2)
    show (X86Data name value end) = printf "%s: db \"%s\", %d, 0" name value end
    show (MOVToMem dest offset reg) = printf "mov [%s-(8*%d)], %s" (show dest) offset (show reg)
    show (MOVPToMem dest offset reg) = printf "mov [%s+(8*%d)], %s" (show dest) offset (show reg)
    show (SUBI reg value) = printf "sub %s, %d" (show reg) value
    show (ADDI reg value) = printf "add %s, %d" (show reg) value
    show (MOVFromMem dest offset source) = printf "mov %s, [%s-(8*%d)]" (show dest) (show source) offset
    show (MOVPFromMem dest offset source) = printf "mov %s, [%s+(8*%d)]" (show dest) (show source) offset
    show (CMPRI reg value) = printf "cmp %s, %d" (show reg) value
    show (JZ label) = printf "jz %s" label
    show (JMP label) = printf "jmp %s" label
    show (Section label) = printf "%s:" label
    show (MOVI reg value) = printf "mov %s, %s" (show reg) (show value)
    show (DIV reg) = printf "div %s" (show reg)
    show (MOVUPS reg label index) = printf "movups %s, [%s + 8 * %d]" (show reg) label index
    show (DoublesArray name values) = printf "%s: dq %s" name (intercalate "," (map show values))
    show (ADDSD left right) = printf "addsd %s, %s" (show left) (show right)
    show (SUBSD left right) = printf "subsd %s, %s" (show left) (show right)
    show (MULSD left right) = printf "mulsd %s, %s" (show left) (show right)
    show (DIVSD left right) = printf "divsd %s, %s" (show left) (show right)
    show (MOVFloatToMem dest offset source) = printf "movsd [%s+(8*%d)], %s" (show dest) offset (show source)
    show (MOVFloatFromMem dest offset source) = printf "movsd %s, [%s+(8*%d)]" 
                                                        (show dest) (show source) offset 
    show (MOVSD dest src) = printf "movsd %s, %s" (show dest) (show src)
    show (PUSHDouble reg) = printf "sub rsp, 8\nmovsd [rsp], %s" (show reg)
    show (POPDouble reg) = printf "movsd %s, [rsp]\nadd rsp, 8" (show reg)

data X86Assembly = X86Assembly {codeSection :: [X86Instruction],
                    dataSection :: [X86Instruction] } deriving Show

mergeAsm :: X86Assembly -> X86Assembly -> X86Assembly
mergeAsm X86Assembly {codeSection = codeSectionOld, dataSection = dataSectionOld}
    X86Assembly {codeSection = codeSectionNew, dataSection = dataSectionNew} = X86Assembly
        {codeSection = codeSectionOld ++ codeSectionNew, dataSection = dataSectionOld ++ dataSectionNew }

getCodeSection :: X86Assembly -> [X86Instruction]
getCodeSection X86Assembly {codeSection = codeSection} = codeSection

getDataSection :: X86Assembly -> [X86Instruction]
getDataSection X86Assembly {dataSection = dataSection} = dataSection

getEmptyX86Asm :: X86Assembly
getEmptyX86Asm = X86Assembly {codeSection = [], dataSection = []}

addCodeSection :: X86Assembly -> [X86Instruction] -> X86Assembly
addCodeSection existing codeSection = mergeAsm existing (X86Assembly
                                                {dataSection = [], codeSection = codeSection})

addDataSection :: X86Assembly -> [X86Instruction] -> X86Assembly
addDataSection existing dataSection = mergeAsm existing (X86Assembly
                                                {dataSection = dataSection, codeSection = []})

getX86Assembly :: [X86Instruction] -> X86Assembly
getX86Assembly = addCodeSection getEmptyX86Asm

mergeMultipleAsm :: [X86Assembly] -> X86Assembly 
mergeMultipleAsm asms = 
    let codes = map getCodeSection asms in 
    let datas = map getDataSection asms in 
    X86Assembly {codeSection = concat codes, dataSection = concat datas}