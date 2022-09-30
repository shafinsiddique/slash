module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly
import Generator.SymbolTable
import Generator.ProgramState
import Text.Printf
import GHC.Generics (Constructor(conName))
import Parser.ReturnType
integerFormattedStringConst = "__slash_integer_format"

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf",
                                                                    Extern "_strcmp",
                                                                    StartMain, PUSH RBP,
                                                                    MOVR RBP RSP] in
    X86Assembly {dataSection =
        [DataSection, X86Data integerFormattedStringConst "%d" 0Xa], codeSection = codeSection}


getEndingAsm :: X86Assembly
getEndingAsm = X86Assembly {dataSection = [], codeSection = [POP RBP, MOV RAX "0", RET]}

getDataConstName :: Integer -> String
getDataConstName = printf "const_%d"

printMath :: Expression -> X86Assembly
printMath expr = let register = R8 in
    let mathOp = getMathExprAsm register expr in
    addCodeSection mathOp [MOV RDI integerFormattedStringConst, MOVR RSI register, CALL "_printf"]

-- Add a const to the data section
-- move the const to the register.
getStringAsm :: String -> Register -> ProgramState -> (X86Assembly, ProgramState)
getStringAsm str destination state =
    let newState = createNewConst state in
    let newId = getConstId newState in
    let strName = getDataConstName newId in
    let dataSection = [X86Data strName str 0xA] in
    let codeSection = [MOV destination strName] in
    (X86Assembly {dataSection = dataSection, codeSection = codeSection}, newState)

printStr :: String -> ProgramState -> (X86Assembly, ProgramState)
printStr str state = let reg = R8 in
    let (stringAsm, newState) = getStringAsm str reg state in
    let code = [MOVR RDI reg, CALL "_printf"]
    in (addCodeSection stringAsm code, newState)

printVarBasedOnTypeAsm :: X86Assembly -> ReturnType -> Register ->  X86Assembly
printVarBasedOnTypeAsm asm varType reg =
    let callPrint = CALL "_printf" in
    let codeSection = case varType of
            IntReturn -> [MOV RDI integerFormattedStringConst, MOVR RSI reg, callPrint]
            StringReturn -> [MOVR RDI reg, callPrint]
            _ -> []
    in addCodeSection asm codeSection

printVariable :: String -> ProgramState -> (X86Assembly, ProgramState)
printVariable name state =
    case findVariable state name of
        Just (VariableInfo offset varType) ->
            let reg = R9 in
            let (varAsm, _) = getVariableExprAsm name reg state in
            (printVarBasedOnTypeAsm varAsm varType reg, state)
        Nothing -> (getEmptyX86Asm, state)

getPrintNumAsm :: Register -> X86Assembly
getPrintNumAsm register = addCodeSection getEmptyX86Asm
                    [MOV RDI integerFormattedStringConst, MOVR RSI register, CALL "_printf"]

getPrintStrAsm :: Register -> X86Assembly
getPrintStrAsm register = addCodeSection getEmptyX86Asm
                                            [MOVR RDI register, CALL "_printf"]

getPrintVariableAsm :: Register -> String -> ProgramState -> X86Assembly
getPrintVariableAsm register name state =
    case findVariable state name of
        Just (VariableInfo offset varType) ->
            case varType of
                IntReturn -> getPrintNumAsm register
                StringReturn -> getPrintStrAsm register
                ErrorReturn -> getEmptyX86Asm
        Nothing -> getEmptyX86Asm

getPrintAsm :: Expression -> Register -> ProgramState -> (X86Assembly, ProgramState)
getPrintAsm expr register state  =
    let (exprAsm, finalState) = generateAsmForExpression expr state register in
    let printCode = case expr of
                    IntExpr _ -> getPrintNumAsm register
                    Addition _ _ -> getPrintNumAsm register
                    Subtraction _ _ -> getPrintNumAsm register
                    Multiplication _ _ -> getPrintNumAsm register
                    StringExpr _ -> getPrintStrAsm register
                    VariableExpr name -> getPrintVariableAsm register name state
                    BooleanOpExpr _ -> getPrintNumAsm register
                    _ -> getEmptyX86Asm in
    (mergeAsm exprAsm printCode, finalState)

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


getVariableExprAsm :: String -> Register -> ProgramState -> (X86Assembly, ProgramState)
getVariableExprAsm name reg state =
    case findVariable state name of
        Just (VariableInfo offset _) -> (addCodeSection getEmptyX86Asm [MOVFromMem reg offset RBP], state)
        Nothing -> (getEmptyX86Asm, state)

getLetExprAsm :: String -> Expression -> Expression -> Register -> ProgramState ->
                                                                    (X86Assembly, ProgramState)
getLetExprAsm name value expr reg state =
    let scratchRegister = R8 in
    let (valAsm, valState) = generateAsmForExpression value state scratchRegister in
    let symbolOffset = getNewSymbolOffset valState in
    let stateWithNewSymbol = addSymbol valState name (VariableInfo symbolOffset (getReturnType value)) in
    let addToStackAsm = addCodeSection valAsm [MOVToMem RBP symbolOffset scratchRegister] in
    let (exprAsm, finalState) = generateAsmForExpression expr stateWithNewSymbol reg in
    (mergeAsm addToStackAsm exprAsm, finalState)

getMathEqualityCode :: Register -> Register -> Register -> X86Assembly
getMathEqualityCode leftReg rightReg dest = getX86Assembly [SUB leftReg rightReg, MOVR dest leftReg]

getStrEqualityCode :: Register -> Register -> Register -> X86Assembly
getStrEqualityCode leftReg rightReg dest = getX86Assembly [MOVR RDI leftReg, MOVR RSI rightReg,
                                            CALL "_strcmp", MOVR dest RAX]

getEqualityCode :: Expression -> Register -> Register -> Register ->  X86Assembly
getEqualityCode expr leftReg rightReg dest = case expr of
    IntExpr _ -> getMathEqualityCode leftReg rightReg dest
    Addition _ _ -> getMathEqualityCode leftReg rightReg dest
    Subtraction _ _ -> getMathEqualityCode leftReg rightReg dest
    Multiplication _ _ -> getMathEqualityCode leftReg rightReg dest
    StringExpr _ -> getStrEqualityCode leftReg rightReg dest
    _ -> getEmptyX86Asm

getBooleanExprAsm :: BooleanOp -> Register -> ProgramState -> (X86Assembly, ProgramState)
getBooleanExprAsm expr reg state =
    let (left, right) = case expr of
                        EqualityExpr left right -> (left, right) in
    let leftReg = R10 in
    let rightReg = R11 in
    let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
    let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
    let mergedAsm = mergeAsm leftAsm rightAsm in
    let comparisonCode = getEqualityCode left leftReg rightReg reg in
    (mergeAsm mergedAsm comparisonCode, rightState)

getIfExprAsm :: Expression -> Expression -> Expression -> Register -> ProgramState ->
                                                                    (X86Assembly , ProgramState)

getIfExprAsm cond thenExp elseExp reg state =
    case cond of
        BooleanOpExpr boolExp ->
            (let scratchReg = R10 in
            let (condAsm, condState) = generateAsmForExpression cond state scratchReg in
            let newIf = createNewIf condState in
            let ifId = getIfCounter newIf in
            let ifBranchLabel = printf "if_branch_%d" ifId in
            let elseBranchlabel = printf "else_branch_%d" ifId in
            let continueBranchLabel = printf "continue_%d" ifId in 
            let mainSectionAsm = addCodeSection condAsm [CMPRI scratchReg 0,
                                                            JZ ifBranchLabel, JMP elseBranchlabel] in
            let (ifBranchAsm, ifBranchState) = generateAsmForExpression thenExp newIf reg in
            let (elseBranchAsm, finalState) = generateAsmForExpression elseExp ifBranchState reg in

            let finalIf = mergeMultipleAsm [getX86Assembly [Section ifBranchLabel], ifBranchAsm, 
                                                    getX86Assembly [JMP continueBranchLabel]] in

            let finalElse = mergeMultipleAsm [getX86Assembly [Section elseBranchlabel], 
                                            elseBranchAsm, getX86Assembly [JMP continueBranchLabel]] in

            (mergeMultipleAsm [mainSectionAsm, finalIf, finalElse, getX86Assembly [Section continueBranchLabel]], finalState ))

        _ -> (getEmptyX86Asm, state)


generateAsmForExpression :: Expression -> ProgramState -> Register -> (X86Assembly, ProgramState)
generateAsmForExpression expression state register =
    let mathGenerator = getMathExprAsm register in
        let (newAsm, newState) = (case expression of
                PrintExpr {toPrint = value} ->  getPrintAsm value register state
                IntExpr _ -> (mathGenerator expression, state)
                Addition _ _ -> (mathGenerator expression, state)
                Subtraction _ _ -> (mathGenerator expression, state)
                Multiplication _ _ -> (mathGenerator expression, state)
                Division _ _ -> (mathGenerator expression, state)
                StringExpr value ->  getStringAsm value register state
                LetExpr name value expr -> getLetExprAsm name value expr register state
                VariableExpr name -> getVariableExprAsm name register state
                BooleanOpExpr booleanExpr -> getBooleanExprAsm booleanExpr register state
                IfExpr cond thenExp elseExp -> getIfExprAsm cond thenExp elseExp register state
                _ -> (getEmptyX86Asm, state) ) in
    (newAsm, newState)

getStackAllocationAsm :: Integer -> X86Assembly
getStackAllocationAsm val = addCodeSection getEmptyX86Asm [SUBI RSP val]

getStackCleanupAsm :: Integer -> X86Assembly
getStackCleanupAsm val = addCodeSection getEmptyX86Asm [ADDI RSP val]

getStackSize :: ProgramState -> Integer
getStackSize state = let tableSize = getSymbolTableSize state in
    let stackSize = (((tableSize * 8) `div` 16) + 1) * 16 in stackSize

addStackAsm :: X86Assembly -> ProgramState -> X86Assembly
addStackAsm asm state = let stackSize = getStackSize state in
    mergeAsm (mergeAsm (getStackAllocationAsm stackSize) asm) (getStackCleanupAsm stackSize)

generateAsmForExpressions :: [Expression] -> (X86Assembly, ProgramState) -> (X86Assembly, ProgramState)
generateAsmForExpressions [] (asm, state) = (asm, state)
generateAsmForExpressions (x:xs) (asm, state) = 
    let (newAsm, newState) =  generateAsmForExpression x state R8 in 
        generateAsmForExpressions xs (mergeAsm asm newAsm, newState)

generateX86 :: [Expression] -> X86Assembly
generateX86 expressions =
    let oldAsm = getInitialAsm in 
    let symbolTable = getNewSymbolTable in
    let state = getInitialState in
    let (newAsm, finalState) = generateAsmForExpressions expressions (getEmptyX86Asm, state) in
    let finalAsm = addStackAsm newAsm finalState in
    mergeMultipleAsm [oldAsm, finalAsm, getEndingAsm]

-- Next Steps : 
-- -- Let In working, parse entire source files rather than snippets, Actually generate code, 

{-

How would I represent symbol tables,

it is a dictionary.

Every function will have it's own. 

When we create a function, we get the functions asm, the symbol table is for that function. 

So every function we created will get passed a symbol table. 

For now, let's just have it a part of our assembly syntax. 


DESIGN : 


We don't manipulate code. 
Code is always merged and is stateless, it's too much hassle having it mutate through function calls.
The caller will have some Assembly.
Calls a function - gets some Assembly.
merge.  



-}
