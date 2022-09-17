module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly 
import Text.Printf

getInitialAsm :: X86Assembly 
getInitialAsm = let codeSection = [Global "_main", Default "rel", Extern "printf"] in
    X86Assembly {dataSection = [], codeSection = codeSection}

getDataConstName :: X86Assembly -> String
getDataConstName (X86Assembly {dataSection = dataSection}) = 
                        printf "const_%s" (show (length dataSection))
                        
getPrintAsm :: String -> X86Assembly -> X86Assembly 
getPrintAsm str existing = let variableName = getDataConstName existing in  
    let dataSection = [X86Data {variableName = variableName, value = str, end = 0xA}] in 
        let codeSection = [MOV RDI variableName, Call "_printf"] in 
            mergeAsm existing (X86Assembly {dataSection = dataSection, codeSection = codeSection})

generateX86 :: Expression -> X86Assembly
generateX86 expression = let initialAsm = getInitialAsm in 
    case expression of
        PrintExpr {toPrint = value} -> getPrintAsm value initialAsm
