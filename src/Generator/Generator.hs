module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly
import Generator.SymbolTable
import Generator.ProgramState
import Text.Printf
import GHC.Generics (Constructor(conName))
import Parser.ReturnType

integerFormattedStringConst = "__slash_integer_format"
doubleFormattedStringConst = "__slash_double_format"
doublesArrayConst = "__slash_doubles_array"

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf",
                                                                    Extern "_strcmp",
                                                                    StartMain, PUSH RBP,
                                                                    MOVR RBP RSP] in
    X86Assembly {dataSection =
        [DataSection,
        X86Data integerFormattedStringConst "%d" 0Xa,
        X86Data doubleFormattedStringConst "%f" 0xa], codeSection = codeSection}


getEndingAsm :: X86Assembly
getEndingAsm = X86Assembly {dataSection = [], codeSection = [POP RBP, MOV RAX "0", RET]}

getDataConstName :: Integer -> String
getDataConstName = printf "const_%d"

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
getPrintNumAsm register = if registerIsForDoubles register
                then getX86Assembly [MOV RDI doubleFormattedStringConst, MOVI RAX 1, CALL "_printf"]
                else getX86Assembly
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

getAsmOrAddToRemaining :: Expression -> [Register] -> [Expression] -> [X86Assembly] -> ProgramState ->
    ([X86Assembly], [Expression], [Register], ProgramState)

getAsmOrAddToRemaining expr [] remaining asms state = (asms, expr:remaining, [], state)
getAsmOrAddToRemaining expr (reg:remainingReg) remaining asms state =
                let (newAsm, newState) = generateAsmForExpression expr state reg in
                (newAsm:asms, remaining, remainingReg, newState)

getPrintExprsAsms :: [Expression] -> [Expression] -> [Register] -> [Register] -> Int ->
    [X86Assembly] -> ProgramState -> ([X86Assembly], [Expression], Int, ProgramState)

getPrintExprsAsms [] remaining _ _ count asms state = (reverse asms, reverse remaining, count, state)

getPrintExprsAsms (x:xs) remaining regular doubles count asms state =
    if expressionHasDouble x then
        let (newAsms, remainingExprs, remainingDoubleReg, newState) =
                        getAsmOrAddToRemaining x doubles remaining asms state in
        let newCount = count + 1 in
        getPrintExprsAsms xs remainingExprs regular remainingDoubleReg newCount newAsms newState

    else
        let (newAsms, remainingExprs, remainingReg, newState) = getAsmOrAddToRemaining x regular remaining asms state in
            getPrintExprsAsms xs remainingExprs remainingReg doubles count newAsms newState

evaluateExprStoreToStack :: Expression -> Register -> Register ->
    Int -> ProgramState -> (X86Assembly, ProgramState)
evaluateExprStoreToStack expression regularReg doubleReg offset state =
        if expressionHasDouble expression then
               let (newAsm, newState) = generateAsmForExpression expression state doubleReg in
                (mergeMultipleAsm [newAsm, getX86Assembly [MOVFloatToMem RSP offset doubleReg ]], newState)

            else
                let (newAsm, newState) = generateAsmForExpression expression state regularReg in
                (mergeMultipleAsm [newAsm, getX86Assembly [MOVPToMem RSP offset regularReg]], newState)


getRemainingExprsToStackAsm :: [Expression] -> ProgramState -> (X86Assembly, ProgramState)
getRemainingExprsToStackAsm expressions state =
    let regReg = R8 in
    let doubleReg = DoubleReg XMM0 in
    let (newAsm, _, newState) = foldl (\(asm, offset, state) expr ->
            let (newAsm, newState) = evaluateExprStoreToStack expr regReg doubleReg offset state in
                (mergeAsm asm newAsm, offset + 1, newState)) (getEmptyX86Asm, 0, state) expressions in
    (mergeMultipleAsm 
        [getX86Assembly [PUSH regReg, PUSHDouble doubleReg], 
        newAsm, getX86Assembly [POPDouble doubleReg, POP regReg]], newState)
-- TOOD: Handle Stack Math.

getPrintAsm :: String -> [Expression] -> Register -> ProgramState -> (X86Assembly, ProgramState)
getPrintAsm str expressions register state =
    let (strAsm, newState) = getStringAsm str RDI state in
        let (exprAsms, remaining, doublesCount, secondState) = getPrintExprsAsms expressions [] [RSI, RDX, RCX, R8, R9] (map DoubleReg [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]) 0 [] newState in
        let (remainingAsm, finalState) = getRemainingExprsToStackAsm remaining secondState in
        let stackSize = toInteger ((((length remaining * 8) `div` 16) + 1) * 16) in
           (mergeMultipleAsm [getX86Assembly [SUBI RSP stackSize], strAsm, mergeMultipleAsm exprAsms, remainingAsm, getX86Assembly [ADDI RSP stackSize, MOVI RAX (toInteger doublesCount), CALL "_printf"]], finalState)

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

registerIsForDoubles :: Register -> Bool
registerIsForDoubles (DoubleReg _) = True
registerIsForDoubles _ = False

getIntExprAsm :: Integer -> Register -> ProgramState -> (X86Assembly, ProgramState)
getIntExprAsm value reg state =
    if registerIsForDoubles reg then getDoubleExprAsm (fromIntegral value) reg state else
    (getX86Assembly [MOVI reg value], state)


getMathExprDoubleAsm :: Expression -> Register -> ProgramState -> (X86Assembly, ProgramState)
getMathExprDoubleAsm expr reg state =
    let (left, right, instr) =
            case expr of
                Addition left right -> (left, right, ADDSD)
                Subtraction left right -> (left, right, SUBSD)
                Multiplication left right -> (left, right, MULSD)
                Division left right -> (left, right, DIVSD) in
            let leftReg = DoubleReg XMM0 in
            let rightReg = DoubleReg XMM1 in
            let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
            let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
            (mergeMultipleAsm [getX86Assembly [SUBI RSP 16, MOVFloatToMem RSP 0 leftReg, MOVFloatToMem RSP 1 rightReg], leftAsm, getX86Assembly
                        [SUBI RSP 8, MOVFloatToMem RSP 0 leftReg], rightAsm, getX86Assembly [MOVFloatFromMem leftReg 0 RSP, ADDI RSP 8, instr leftReg rightReg, MOVSD reg leftReg,
                        MOVFloatFromMem leftReg 0 RSP, MOVFloatFromMem rightReg 1 RSP]], rightState)

getMathExprAsm2 :: Expression -> Register -> ProgramState -> (X86Assembly, ProgramState)
getMathExprAsm2 expr reg state =
    if registerIsForDoubles reg then getMathExprDoubleAsm expr reg state
    else
        (let (left, right, instr) =
                case expr of
                    Addition left right -> (left, right, ADD )
                    Subtraction left right -> (left, right, SUB  )
                    Multiplication left right -> (left, right, IMUL) in
        let leftReg = R8 in
        let rightReg = R9 in
        let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
        let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
        (mergeMultipleAsm
                [getX86Assembly [PUSH leftReg, PUSH rightReg],
                leftAsm, getX86Assembly [PUSH leftReg], rightAsm,
                getX86Assembly [POP leftReg, instr leftReg rightReg,
                            MOVR reg leftReg, POP rightReg, POP leftReg]], rightState))

getDivExprAsm :: Expression -> Expression -> Register -> ProgramState -> (X86Assembly, ProgramState)
getDivExprAsm left right reg state =
    if registerIsForDoubles reg then getMathExprDoubleAsm (Division left right) reg state
    else
        let leftReg = R8 in
        let rightReg = R9 in
        let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
        let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
        let ending = getX86Assembly [POP leftReg, PUSH RDX, MOVI RDX 0, PUSH RAX, MOVR RAX leftReg,
                DIV rightReg, MOVR reg RAX, POP RAX, POP RDX, POP rightReg, POP leftReg] in
        (mergeMultipleAsm [getX86Assembly [PUSH leftReg, PUSH rightReg], leftAsm, getX86Assembly [PUSH leftReg], rightAsm, ending], rightState)

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


getDoubleExprAsm :: Double -> Register -> ProgramState -> (X86Assembly, ProgramState)
getDoubleExprAsm value reg state =
    let newState = addDouble state value in
    let asm = getX86Assembly [MOVUPS reg doublesArrayConst (findDouble newState value)] in
    (asm, newState)

generateAsmForExpression :: Expression -> ProgramState -> Register -> (X86Assembly, ProgramState)
generateAsmForExpression expression state register =
    let (newAsm, newState) =
            (case expression of
                PrintExpr {toPrint = value, expressions = exprs} ->  getPrintAsm value exprs register state
                IntExpr value -> getIntExprAsm value register state
                Addition _ _ -> getMathExprAsm2 expression register state
                Subtraction _ _ -> getMathExprAsm2 expression register state
                Multiplication _ _ -> getMathExprAsm2 expression register state
                Division left right -> getDivExprAsm left right register state
                StringExpr value ->  getStringAsm value register state
                LetExpr name value expr -> getLetExprAsm name value expr register state
                VariableExpr name -> getVariableExprAsm name register state
                BooleanOpExpr booleanExpr -> getBooleanExprAsm booleanExpr register state
                IfExpr cond thenExp elseExp -> getIfExprAsm cond thenExp elseExp register state
                DoubleExpr value -> getDoubleExprAsm value register state) in
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

expressionHasDouble :: Expression -> Bool
expressionHasDouble (DoubleExpr _) = True
expressionHasDouble (Addition left right) = expressionHasDouble left || expressionHasDouble right
expressionHasDouble (Subtraction left right) = expressionHasDouble left || expressionHasDouble right
expressionHasDouble (Division left right) = expressionHasDouble left || expressionHasDouble right
expressionHasDouble (Multiplication left right) = expressionHasDouble left || expressionHasDouble right
expressionHasDouble _ = False

getOutputRegister :: Expression -> Register
getOutputRegister expr = if expressionHasDouble expr then DoubleReg XMM0 else R8

generateAsmForExpressions :: [Expression] -> (X86Assembly, ProgramState) -> (X86Assembly, ProgramState)
generateAsmForExpressions [] (asm, state) = (asm, state)
generateAsmForExpressions (x:xs) (asm, state) =
    let (newAsm, newState) =  generateAsmForExpression x state (getOutputRegister x) in
        generateAsmForExpressions xs (mergeAsm asm newAsm, newState)

generateX86 :: [Expression] -> X86Assembly
generateX86 expressions =
    let oldAsm = getInitialAsm in
    let symbolTable = getNewSymbolTable in
    let state = getInitialState in
    let (newAsm, finalState) = generateAsmForExpressions expressions (getEmptyX86Asm, state) in
    let finalAsm = addStackAsm newAsm finalState in
    let asmWithDoubles = addDataSection finalAsm
                                [DoublesArray doublesArrayConst (getDoubleValues finalState)] in
    mergeMultipleAsm [oldAsm, asmWithDoubles, getEndingAsm]

