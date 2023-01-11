module Generator.X86Assembly where

import Foreign (touchForeignPtr)
import Text.Printf
import Generator.SymbolTable
import Data.List

data DoubleRegister = XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7

data SingleByteReg = AL | DIL | SIL | DL | CL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B 
                    | SPL | BL | BPL 

data TwoByteReg = AX | DI | SI | DX | CX | R8W | R9W | R10W | SP | BX | BP  
            | R11W | R12W | R13W | R14W | R15W

data FourByteReg = EAX | EDI | ESI | EDX  | ECX | R8D | R9D 
            | R10D | R11D | R12D | R13D | R14D | R15D | ESP | EBX | EBP

data WordReg = RDI | RSI | RBP | RAX | R8 | R9 | RSP | R10 | R11 | RCX | RDX | RBX | R12 
                    | R13 | R14 | R15 |
                    DoubleReg DoubleRegister 

data Register =  WR WordReg | SB SingleByteReg | TB TwoByteReg
                    | FB FourByteReg

instance Show SingleByteReg where
    show AL = "al"
    show DIL = "dil"
    show SIL = "sil"
    show DL = "dl"
    show CL = "cl"
    show R8B = "r8b"
    show R9B = "r9b"
    show R10B = "r10b"
    show R11B = "r11b"
    show R12B = "r12b"
    show R13B = "r13b"
    show R14B = "r14b"
    show R15B = "r15b"
    show SPL = "spl"
    show BL = "bl"
    show BPL = "bpl"

instance Show TwoByteReg where
    show AX = "ax"
    show DI = "di"
    show SI = "SI"
    show DX = "dx"
    show CX = "cx"
    show R8W = "r8w"
    show R9W = "r9w"
    show R10W = "r10w"
    show R11W = "r11w"
    show SP = "sp"
    show BX = "bx"
    show BP = "bp"
    show R12W = "r12w"
    show R13W = "r13w"
    show R14W = "r14w"
    show R15W = "r15w"

instance Show FourByteReg where
    show EAX = "eax"
    show EDI = "edi"
    show ESI = "esi"
    show EDX = "ecx"
    show R8D = "r8d"
    show R9D = "r9d"
    show R10D = "R10D"
    show R11D = "r11d"
    show ECX = "ecx"
    show ESP = "esp"
    show R12D = "r12d"
    show R13D = "r13d"
    show R14D = "r14d"
    show R15D = "r15d"
    show EBX = "ebx"
    show EBP = "ebp"

instance Show DoubleRegister where
    show XMM0 = "xmm0"
    show XMM1 = "xmm1"
    show XMM2 = "xmm2"
    show XMM3 = "xmm3"
    show XMM4 = "xmm4"
    show XMM5 = "xmm5"
    show XMM6 = "xmm6"
    show XMM7 = "xmm7"
 

instance Show WordReg where
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
    show RBX = "rbx"
    show R12 = "r12"
    show R13 = "r13"
    show R14 = "r14"
    show R15 = "r15"
    show (DoubleReg reg)  = show reg


instance Show Register where
    show (WR reg) = show reg
    show (SB reg) = show reg
    show (TB reg) = show reg
    show (FB reg) = show reg
    

data X86Instruction = MOV Register String | CALL String |
        Extern String | Global String | Default String | StringPair String String
        | ADD Register Register | SUB Register Register | IMUL Register Register
        | PUSH Register | POP Register | TextSection | DataSection | RET | StartMain
        | MOVR Register Register
        | X86Data String String Integer
        | X86StringConst String String
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
        | MOVMFloatToMem Register Int Register
        | MOVFloatFromMem Register Int Register 
        | MOVMFloatFromMem Register Int Register 
        | MOVSD Register Register
        | MOVPToMem Register Int Register
        | PUSHDouble Register
        | POPDouble Register
        | PUSH32 Register
        | POP32 Register
        | MOVToStack Integer Register
        | MOVDoubleToStack Integer Register
        | MOVFromStack Register Integer
        | MOVDoubleFromStack Register Integer
        | PUSHBytes Integer Register 
        | POPBytes Register Integer
        | XOR Register Register
        | DEBUG String

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
    show (X86StringConst name value) = printf "%s: db `%s`, 10, 0" name value
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
    show (DoublesArray name values) = case values of
        [] -> ""
        values -> printf "%s: dq %s" name (intercalate "," (map show values))
    show (ADDSD left right) = printf "addsd %s, %s" (show left) (show right)
    show (SUBSD left right) = printf "subsd %s, %s" (show left) (show right)
    show (MULSD left right) = printf "mulsd %s, %s" (show left) (show right)
    show (DIVSD left right) = printf "divsd %s, %s" (show left) (show right)
    show (MOVFloatToMem dest offset source) = printf "movsd [%s+(8*%d)], %s" (show dest) offset (show source)
    show (MOVMFloatToMem dest offset source) = printf "movsd [%s-(8*%d)], %s" (show dest) offset (show source)
    show (MOVFloatFromMem dest offset source) = printf "movsd %s, [%s+(8*%d)]" 
                                                        (show dest) (show source) offset 

    show (MOVMFloatFromMem dest offset source) = printf "movsd %s, [%s-(8*%d)]" 
                                                        (show dest) (show source) offset                                                  
    show (MOVSD dest src) = printf "movsd %s, %s" (show dest) (show src)
    show (PUSHDouble reg) = printf "sub rsp, 8\nmovsd [rsp], %s" (show reg)
    show (POPDouble reg) = printf "movsd %s, [rsp]\nadd rsp, 8" (show reg)
    show (PUSH32 reg) = printf "sub rsp, 4\nmov [rsp], %s" (show reg)
    show (POP32 reg) = printf "mov %s, [rsp]\nadd rsp, 4" (show reg)
    show (MOVToStack offset src) = printf "mov [rbp-%d], %s"  offset (show src)
    show (MOVDoubleToStack offset src) =  printf "movsd [rbp-%d], %s"  offset (show src)
    show (MOVFromStack reg offset) = printf "mov %s, [rbp-%d]" (show reg) offset
    show (MOVDoubleFromStack reg offset) = printf "movsd %s, [rbp-%d]" (show reg) offset
    show (PUSHBytes bytes fromReg) = printf "%s\nmov [rsp], %s" (show (SUBI (WR RSP) bytes)) (show fromReg)
    show (POPBytes reg bytes) = printf "mov %s, [rsp]\n" (show reg) (show (ADDI (WR RSP) bytes))
    show (XOR reg1 reg2) = printf "xor %s, %s" (show reg1) (show reg2)
    show (DEBUG val) = val
    
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