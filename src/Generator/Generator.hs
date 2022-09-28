module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly
import Text.Printf

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf", 
                                                                    StartMain, Push RBP] in
    X86Assembly {dataSection = [], codeSection = codeSection}

getEndingAsm :: X86Assembly
getEndingAsm = X86Assembly {dataSection = [], codeSection = [Pop RBP, MOV RAX "0", Ret]}

getDataConstName :: X86Assembly -> String
getDataConstName X86Assembly {dataSection = dataSection} =
                        printf "const_%s" (show (length dataSection))

getPrintAsm :: String -> Register -> X86Assembly -> X86Assembly
getPrintAsm str register existing  = let variableName = getDataConstName existing in
    let dataSection = [X86Data {variableName = variableName, value = str, end = 0xA}] in
        let codeSection = [MOV RDI variableName, Call "_printf", MOV register "0"] in
            mergeAsm existing (X86Assembly {dataSection = dataSection, codeSection = codeSection})

getMathLeftRightAsm :: Register -> Register  -> Expression -> Expression -> X86Assembly
getMathLeftRightAsm leftReg rightReg left right  =
                let leftAsm = getMathExprAsm leftReg getDefaultX86Asm left in
                let pushToStack = addCodeSection leftAsm [Push leftReg] in 
                let rightAsm = getMathExprAsm rightReg pushToStack right  in
                rightAsm

operateOn :: Expression -> Register -> Register -> Register -> X86Assembly -> X86Assembly
operateOn expr left right destination existing =
    let popLeft = Pop left in
    let moveInstr = MOV destination (show left) in 
    let instruction = (case expr of
                        Addition _ _ -> [popLeft, Add left right, moveInstr]
                        Subtraction _ _ -> [popLeft, Sub left right, moveInstr]
                        Multiplication _ _ -> [popLeft, IMul left right, moveInstr]
                        _ -> []) in
    addCodeSection existing instruction


getMathExprAsm :: Register -> X86Assembly -> Expression -> X86Assembly
getMathExprAsm register existing expr  =
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
                        _ -> existing) in
                mergeAsm existing (operateOn expr leftReg rightReg register asm)


generateX86 :: Expression -> X86Assembly
generateX86 expression = let initialAsm = getInitialAsm in
    let mathGenerator = getMathExprAsm R8 initialAsm in
    let middle = (case expression of
                PrintExpr {toPrint = value} -> getPrintAsm value R8 initialAsm
                IntExpr _ -> mathGenerator expression
                Addition _ _ -> mathGenerator expression
                Subtraction _ _ -> mathGenerator expression
                Multiplication _ _ -> mathGenerator expression
                Division _ _ -> mathGenerator expression
                _ -> initialAsm) 
        in mergeAsm middle getEndingAsm

-- how is this gonna work?
-- We pass in the register where it must be stored.
-- Given that register, it simply stores it in that register. That's it.

