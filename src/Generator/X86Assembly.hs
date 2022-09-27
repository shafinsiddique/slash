{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Generator.X86Assembly where

import Foreign (touchForeignPtr)
import Text.Printf

data Register = RDI | RSI 

instance Show Register where 
    show RDI = "rdi"
    show RSI = "rsi"


data X86Data = X86Data {variableName :: String, value :: String, end :: Integer} deriving Show

data X86Instruction = MOV Register String | Call String | 
        Extern String | Global String | Default String | StringPair String String 
        | Add Register Register 
        

instance Show X86Instruction where
    show (MOV reg value) = printf "mov %s, %s" (show reg) value
    show (Call value) = printf "call %s" value
    show (Extern value) = printf "extern %s" value
    show (Default value) = printf "default %s" value
    show (Global value) = printf "global %s" value
    show (StringPair val1 val2) = printf "%s %s" val1 val2
    show (Add reg1 reg2) = printf "add %s, %s" (show reg1) (show reg2)

data X86Assembly = X86Assembly {codeSection :: [X86Instruction], dataSection :: [X86Data] } deriving Show

mergeAsm :: X86Assembly -> X86Assembly -> X86Assembly 
mergeAsm (X86Assembly {codeSection = codeSectionOld, dataSection = dataSectionOld}) 
    (X86Assembly {codeSection = codeSectionNew, dataSection = dataSectionNew}) = X86Assembly 
        {codeSection = codeSectionOld ++ codeSectionNew, dataSection = dataSectionOld ++ dataSectionNew }

getDefaultX86Asm :: X86Assembly
getDefaultX86Asm = X86Assembly {codeSection = [], dataSection = []}

addCodeSection :: X86Assembly -> [X86Instruction] -> X86Assembly
