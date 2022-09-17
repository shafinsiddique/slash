{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Generator.X86Assembly where
import Foreign (touchForeignPtr)

data Register = RDI | RSI deriving Show

data X86Data = X86Data {variableName :: String, value :: String, end :: Integer} deriving Show
data X86Instruction = MOV Register String | Call String deriving Show

data X86Assembly = X86Assembly {codeSection :: [X86Instruction], dataSection :: [X86Data] } deriving Show

