module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly
import Text.Printf

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf", 
                                                                    StartMain, PUSH RBP] in
    X86Assembly {dataSection = [], codeSection = codeSection}

getEndingAsm :: X86Assembly
getEndingAsm = X86Assembly {dataSection = [], codeSection = [POP RBP, MOV RAX "0", RET]}

getDataConstName :: X86Assembly -> String
getDataConstName X86Assembly {dataSection = dataSection} =
                        printf "const_%s" (show (length dataSection))

getPrintAsm :: String -> Register -> X86Assembly -> X86Assembly
getPrintAsm str register existing  = let variableName = getDataConstName existing in
    let dataSection = [X86Data {variableName = variableName, value = str, end = 0xA}] in
        let codeSection = [MOV RDI variableName, CALL "_printf", MOV register "0"] in
            X86Assembly {dataSection = dataSection, codeSection = codeSection}

getMathLeftRightAsm :: Register -> Register  -> Expression -> Expression -> X86Assembly
getMathLeftRightAsm leftReg rightReg left right  =
                let leftAsm = addCodeSection (getMathExprAsm leftReg left)[PUSH leftReg] in
                let rightAsm = getMathExprAsm rightReg right  in
                mergeAsm leftAsm rightAsm

operateOn :: Expression -> Register -> Register -> Register -> X86Assembly -> X86Assembly
operateOn expr left right destination existing =
    let popLeft = POP left in
    let moveInstr = MOV destination (show left) in 
    let instruction = (case expr of
                        Addition _ _ -> [popLeft, ADD left right, moveInstr]
                        Subtraction _ _ -> [popLeft, SUB left right, moveInstr]
                        Multiplication _ _ -> [popLeft, IMUL left right, moveInstr]
                        _ -> []) in
    addCodeSection existing instruction

getMathExprAsm :: Register -> Expression -> X86Assembly
getMathExprAsm register expr  =
                let leftReg = R8 in
                let rightReg = R9 in
                let helper = getMathLeftRightAsm leftReg rightReg in
                let asm = (case expr of
                        IntExpr value -> X86Assembly {dataSection = [], codeSection =
                                                            [MOV register (show value)]}
                        Addition left right -> helper left right
                        Subtraction left right -> helper left right
                        Multiplication left right -> helper left right
                        Division left right -> helper left right
                        _ -> getEmptyX86Asm) in
                operateOn expr leftReg rightReg register asm

generateX86 :: Expression -> X86Assembly ->  X86Assembly
generateX86 expression oldAsm = 
    let mathGenerator = getMathExprAsm R8 in
    let newAsm = (case expression of
                PrintExpr {toPrint = value} -> getPrintAsm value R8 oldAsm
                IntExpr _ -> mathGenerator expression
                Addition _ _ -> mathGenerator expression
                Subtraction _ _ -> mathGenerator expression
                Multiplication _ _ -> mathGenerator expression
                Division _ _ -> mathGenerator expression
                _ -> oldAsm) 
        in mergeAsm oldAsm newAsm

-- how is this gonna work?
-- We pass in the register where it must be stored.
-- Given that register, it simply stores it in that register. That's it.

