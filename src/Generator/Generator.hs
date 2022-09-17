module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly 
import Text.Printf

getPrintAsm :: String -> X86Assembly 
getPrintAsm str = let variableName = printf "const.1" in  
    let dataSection = [X86Data {variableName = variableName, value = str, end = 0xA}] in 
        let codeSection = [MOV RDI variableName, Call "_printf"] in 
            X86Assembly {dataSection = dataSection, codeSection = codeSection}

generateX86 :: Expression -> X86Assembly
generateX86 PrintExpr {toPrint = str} = getPrintAsm str
