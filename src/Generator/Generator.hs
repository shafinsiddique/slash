module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly
import Generator.SymbolTable
import Generator.ProgramState
import Text.Printf
import GHC.Generics (Constructor(conName))
import Parser.ReturnType
import Data.Foldable
import Data.Map
import Data.Maybe
doublesArrayConst = "__slash_doubles_array"

data RegisterDivisons = RegisterDivision
                            {one :: Register,
                            two :: Register,
                            four :: Register,
                            eight :: Register}

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf",
                                                                    Extern "_strcmp",
                                                                    StartMain, PUSH RBP,
                                                                    MOVR RBP RSP] in
    X86Assembly {dataSection =
        [DataSection], codeSection = codeSection}


getEndingAsm :: ProgramState -> X86Assembly
getEndingAsm state = X86Assembly {dataSection = [], codeSection =
        [ADDI RSP (getBytesAllocated state), POP RBP, MOV RAX "0", RET]}

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

getRegisterSize :: Register -> Integer
getRegisterSize (DoubleReg _) = 8
getRegisterSize (SingleByteReg _) = 1
getRegisterSize _ = 8

getAsmOrAddToRemaining :: Expression -> [Register] -> [Expression] -> [X86Assembly] -> ProgramState -> [X86Instruction] -> ([X86Assembly], [Expression], [Register], ProgramState, [X86Instruction])

getAsmOrAddToRemaining expr [] remaining asms state pops = (asms, expr:remaining, [], state, pops)
getAsmOrAddToRemaining expr (reg:remainingReg) remaining asms state pops =
                let regForDoubles = registerIsForDoubles reg in
                let exprSize = 8 in
                let pushInstr = if regForDoubles then PUSHDouble else PUSH in
                let originalBytes = getBytesAllocated state in 
                let (newAsm, newState) = generateAsmForExpression expr state reg in
                let bytesAllocated = getBytesAllocated newState in 
                let finalState = setBytes newState originalBytes in 
                let finalAsm = addCodeSection newAsm [ADDI RSP (bytesAllocated-originalBytes), pushInstr reg] in
                let popInstr = if regForDoubles then POPDouble else POP in
                (finalAsm:asms, remaining, remainingReg, addBytes finalState exprSize, popInstr reg:pops)

getPrintExprsAsms :: [Expression] -> [Expression] -> [Register] -> [Register] -> Int ->
    [X86Assembly] -> ProgramState -> [X86Instruction] -> Integer -> ([X86Assembly], [Expression], Int, ProgramState, [X86Instruction])

getPrintExprsAsms [] remaining _ _ count asms state pops originalBytes = (reverse asms, reverse remaining, count, setBytes state originalBytes, pops)

getPrintExprsAsms (x:xs) remaining regular doubles count asms state pops originalBytes  =
    if expressionHasDouble x state then
        let (newAsms, remainingExprs, remainingDoubleReg, newState, newPops) =
                        getAsmOrAddToRemaining x doubles remaining asms state pops in
        let newCount = count + 1 in
        getPrintExprsAsms xs remainingExprs regular remainingDoubleReg newCount newAsms newState newPops originalBytes

    else
        let (newAsms, remainingExprs, remainingReg, newState, newPops) = getAsmOrAddToRemaining x regular remaining asms state pops in
            getPrintExprsAsms xs remainingExprs remainingReg doubles count newAsms newState newPops originalBytes

evaluateExprStoreToStack :: Expression -> Register -> Register ->
    Int -> ProgramState -> (X86Assembly, ProgramState)
evaluateExprStoreToStack expression regularReg doubleReg offset state =
        if expressionHasDouble expression state then
               let (newAsm, newState) = generateAsmForExpression expression state doubleReg in
                (mergeMultipleAsm [newAsm, getX86Assembly [MOVFloatToMem RSP offset doubleReg ]], newState)

            else
                let (newAsm, newState) = generateAsmForExpression expression state regularReg in
                (mergeMultipleAsm [newAsm, getX86Assembly [MOVPToMem RSP offset regularReg]], newState)


getRemainingExprsToStackAsm :: [Expression] -> Register -> Register -> ProgramState -> (X86Assembly, ProgramState)
getRemainingExprsToStackAsm expressions regularReg doubleReg state =
    let (newAsm, _, newState) = Data.Foldable.foldl (\(asm, offset, state) expr ->
            let (newAsm, newState) = evaluateExprStoreToStack expr regularReg doubleReg offset state in
                (mergeAsm asm newAsm, offset + 1, newState)) (getEmptyX86Asm, 0, state) expressions in
    (newAsm, newState)

getDivRegistersAsm :: Register -> Register -> X86Assembly
getDivRegistersAsm left right = getX86Assembly [MOVR RAX left, MOVI RDX 0, DIV right]

{-

    variables 
    push 
    variables
    push 

-}
getPrintAsm :: String -> [Expression] -> Register -> ProgramState -> (X86Assembly, ProgramState)
getPrintAsm str expressions register state =
    let (strAsm, newState) = getStringAsm str RDI state in
        let originalBytes = getBytesAllocated state in
        let (exprAsms, remaining, doublesCount, secondState, pops) = getPrintExprsAsms expressions [] [RSI, RDX, RCX, R8, R9] (Prelude.map DoubleReg [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]) 0 [] newState [] originalBytes in
        let regularReg = R8 in
        let doubleReg = DoubleReg XMM0 in
        let (remainingAsm, finalState) =
                        getRemainingExprsToStackAsm remaining regularReg doubleReg secondState in
        let stackSize = toInteger ((((length remaining * 8) `div` 16) + 1) * 16) in
        let stackTopOffset = fromIntegral stackSize `div` 8 in
           (mergeMultipleAsm [strAsm, addCodeSection (mergeMultipleAsm exprAsms) pops,
                        getX86Assembly [MOVR R11 RDX, MOVI R10 16], getDivRegistersAsm RSP R10,
                        getX86Assembly [SUB RSP RDX, SUBI RSP 32, MOVPToMem RSP 0 regularReg, MOVFloatToMem RSP 1 doubleReg, MOVPToMem RSP 2 RDX, SUBI RSP stackSize, MOVR RDX R11],
        remainingAsm, getX86Assembly [MOVPFromMem regularReg stackTopOffset RSP, MOVFloatFromMem doubleReg (stackTopOffset+1) RSP, MOVI RAX (toInteger doublesCount), CALL "_printf", ADDI RSP (stackSize + 16), POP RDX, ADDI RSP 8, ADD RSP RDX]], finalState)

registerIsForDoubles :: Register -> Bool
registerIsForDoubles (DoubleReg _) = True
registerIsForDoubles _ = False

createAndAddSymbol :: ProgramState -> String -> ExpressionType -> ProgramState
createAndAddSymbol = addSymbol

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
            (mergeMultipleAsm [leftAsm, getX86Assembly
                        [SUBI RSP 8, MOVFloatToMem RSP 0 leftReg], rightAsm, getX86Assembly [MOVFloatFromMem leftReg 0 RSP, ADDI RSP 8, instr leftReg rightReg, MOVSD reg leftReg]], rightState)

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
                [leftAsm, getX86Assembly [PUSH leftReg], rightAsm,
                getX86Assembly [POP leftReg, instr leftReg rightReg,
                            MOVR reg leftReg]], rightState))

getDivExprAsm :: Expression -> Expression -> Register -> ProgramState -> (X86Assembly, ProgramState)
getDivExprAsm left right reg state =
    if registerIsForDoubles reg then getMathExprDoubleAsm (Division left right) reg state
    else
        let leftReg = R8 in
        let rightReg = R9 in
        let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
        let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
        let ending = getX86Assembly [POP leftReg, MOVI RDX 0, MOVR RAX leftReg,
                DIV rightReg, MOVR reg RAX] in
        (mergeMultipleAsm [leftAsm, getX86Assembly [PUSH leftReg], rightAsm, ending], rightState)

{-
Expr size can not be bigger than max register size. If it is, the pointer should be stored. 

 let (exprAsms, remaining, doublesCount, secondState, pops) = getPrintExprsAsms expressions [] [RSI, RDX, RCX, R8, R9] 

-}

getRegWithSize :: Integer -> RegisterDivisons -> Register
getRegWithSize exprSize (RegisterDivision one two four eight)
  | exprSize <= 1 = one
  | exprSize <= 2 = two
  | exprSize <= 4 = four
  | otherwise =
    eight

getCorrectReg :: Register -> Integer -> Register
getCorrectReg reg exprSize =
    if exprSize >= (getRegisterSize reg-2) then reg else
        let registerDivisons =
                [RegisterDivision (SingleByteReg DIL) (TwoByteReg DI) (FourByteReg EDI) RDI,
                RegisterDivision (SingleByteReg SIL) (TwoByteReg SI) (FourByteReg ESI) RSI,
                RegisterDivision (SingleByteReg DL) (TwoByteReg DX) (FourByteReg EDX) RDX,
                RegisterDivision (SingleByteReg CL) (TwoByteReg CX) (FourByteReg ECX) RCX,
                RegisterDivision (SingleByteReg R8B) (TwoByteReg R8W) (FourByteReg R8D) R8,
                RegisterDivision (SingleByteReg R9B) (TwoByteReg R9W) (FourByteReg R9D) R9,
                RegisterDivision (SingleByteReg R10B) (TwoByteReg R10W) (FourByteReg R10D) R10] in
                let correspondingDiv = case reg of
                        RDI -> head registerDivisons
                        RSI -> registerDivisons !! 1
                        RDX -> registerDivisons !! 2
                        RCX -> registerDivisons !! 3
                        R8 -> registerDivisons !! 4
                        R9 -> registerDivisons !! 5
                        R10 -> registerDivisons !! 6
                        SingleByteReg val ->
                            case val of
                                DIL -> head registerDivisons
                                SIL -> registerDivisons !! 1
                                DL -> registerDivisons !! 2
                                CL -> registerDivisons !! 3
                                R8B -> registerDivisons !! 4
                                R9B -> registerDivisons !! 5
                                R10B -> registerDivisons !! 6
                        TwoByteReg val ->
                            case val of
                                DI -> head registerDivisons
                                SI -> registerDivisons !! 1
                                DX -> registerDivisons !! 2
                                CX -> registerDivisons !! 3
                                R8W -> registerDivisons !! 4
                                R9W -> registerDivisons !! 5
                                R10W -> registerDivisons !! 6
                        FourByteReg val ->
                            case val of
                                EDI -> head registerDivisons
                                ESI -> registerDivisons !! 1
                                EDX -> registerDivisons !! 2
                                ECX -> registerDivisons !! 3
                                R8D -> registerDivisons !! 4
                                R9D -> registerDivisons !! 5
                                R10D -> registerDivisons !! 6
                    in getRegWithSize exprSize correspondingDiv



{-
If it's a 1 byte data, but we get a  8 byte register,we need the corresponding single byte portion
of this register. This can be calculated easily. THen we ZERO OUT THE ENTIRE REGISTER (since we're only writing to a specific portion, this is important). Then we move it to the appropriate register. 
-}
getVariableExprAsm :: String -> Register -> ProgramState -> (X86Assembly, ProgramState)
getVariableExprAsm name reg state =
    case findVariable state name of
        Just VariableInfo {offset = offset, exprType = exprType} ->
                if registerIsForDoubles reg
                    then (getX86Assembly [MOVDoubleFromStack reg offset], state)
                else
                    let correctReg = getCorrectReg reg (getExprSize exprType) in
                        (getX86Assembly [XOR reg reg, MOVFromStack correctReg offset], state)

            -- (getX86Assembly [instr correctReg offset], state)
        Nothing -> (getEmptyX86Asm, state)

getLetExprAsm :: String -> Expression -> Expression -> Register -> ProgramState ->
                                                                    (X86Assembly, ProgramState)
getLetExprAsm name value expr reg state =
    let exprType = getExprType value state in
    let scratchRegister = getOutputRegisterFromType exprType  in
    let (valAsm, valState) = generateAsmForExpression value state scratchRegister in
    let symbolOffset = getNewSymbolOffset valState exprType in
    let stateWithNewSymbol = addSymbol valState name exprType in
    let movInstr = if registerIsForDoubles scratchRegister then MOVDoubleToStack else MOVToStack in
    let addToStackAsm = addCodeSection valAsm
                [SUBI RSP (getExprSize exprType), movInstr symbolOffset scratchRegister] in
    let (exprAsm, finalState) = generateAsmForExpression expr stateWithNewSymbol reg in
    (mergeAsm addToStackAsm exprAsm, finalState)

getMathEqualityCode :: Register -> Register -> Register -> X86Assembly
getMathEqualityCode leftReg rightReg dest = getX86Assembly [SUB leftReg rightReg, MOVR dest leftReg]

getStrEqualityCode :: Register -> Register -> Register -> X86Assembly
getStrEqualityCode leftReg rightReg dest = getX86Assembly [MOVR RDI leftReg, MOVR RSI rightReg,
                                            CALL "_strcmp", MOVR dest RAX]

getEqualityCode :: Expression -> Register -> Register -> Register -> X86Assembly
getEqualityCode expr leftReg rightReg dest = case expr of
    IntExpr _ -> getMathEqualityCode leftReg rightReg dest
    Addition _ _ -> getMathEqualityCode leftReg rightReg dest
    Subtraction _ _ -> getMathEqualityCode leftReg rightReg dest
    Multiplication _ _ -> getMathEqualityCode leftReg rightReg dest
    StringExpr _ -> getStrEqualityCode leftReg rightReg dest
    _ -> getEmptyX86Asm

{-

println("%b", let x = True in x -- (rdi))

-}
getTrueFalseExprAsm :: Bool -> Register -> ProgramState -> (X86Assembly, ProgramState)
getTrueFalseExprAsm val reg state = let intRepresentation = if val then 1 else 0 in
    (getX86Assembly [MOVI reg intRepresentation], state)

getBooleanExprAsm :: BooleanOp -> Register -> ProgramState -> (X86Assembly, ProgramState)
getBooleanExprAsm expr reg state =
    case expr of
        TrueFalseExpr val -> getTrueFalseExprAsm val reg state
    -- let (left, right) = case expr of
    --                     EqualityExpr left right -> (left, right) in
    -- let leftReg = R10 in
    -- let rightReg = R11 in
    -- let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
    -- let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
    -- let mergedAsm = mergeAsm leftAsm rightAsm in
    -- let comparisonCode = getEqualityCode left leftReg rightReg reg in
    -- (mergeAsm mergedAsm comparisonCode, rightState)

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

_expressionHasDouble :: Expression -> ProgramState -> Bool
_expressionHasDouble (DoubleExpr _) _ = True
_expressionHasDouble (Addition left right) table =
                            _expressionHasDouble left table || _expressionHasDouble right table
_expressionHasDouble (Subtraction left right) table =
                            _expressionHasDouble left table || _expressionHasDouble right table
_expressionHasDouble (Division left right) table =
                            _expressionHasDouble left table || _expressionHasDouble right table
_expressionHasDouble (Multiplication left right) table =
                            _expressionHasDouble left table || _expressionHasDouble right table
_expressionHasDouble (VariableExpr name) state =
    case findVariable state name of
        Just VariableInfo {exprType = exprType} -> case exprType of
            DoubleType -> True
            _ -> False
        Nothing -> False

_expressionHasDouble (LetExpr name value result) state =
    let newState = createAndAddSymbol state name (getExprType value state) in
        _expressionHasDouble result newState

_expressionHasDouble _ _ = False

expressionHasDouble :: Expression -> ProgramState -> Bool
expressionHasDouble = _expressionHasDouble

getExprType :: Expression -> ProgramState -> ExpressionType
getExprType (LetExpr name value result) state =
    let newState = createAndAddSymbol state name (getExprType value state) in
        getExprType result newState
getExprType (VariableExpr name) state = case findVariable state name of
    Just VariableInfo {exprType = exprType} -> exprType
    Nothing -> ErrorType

getExprType (BooleanOpExpr _) state = BoolType

getExprType (StringExpr _) state = StrType

getExprType expr state = if expressionHasDouble expr state then DoubleType else IntType

getOutputRegisterFromType :: ExpressionType  -> Register
getOutputRegisterFromType exprType = case exprType of
    DoubleType -> DoubleReg XMM0
    BoolType -> SingleByteReg R8B
    _ -> R8

getOutputRegister :: Expression -> ProgramState -> Register
getOutputRegister expr state = getOutputRegisterFromType (getExprType expr state)


generateAsmForExpressions :: [Expression] -> (X86Assembly, ProgramState) -> (X86Assembly, ProgramState)
generateAsmForExpressions [] (asm, state) = (asm, state)
generateAsmForExpressions (x:xs) (asm, state) =
    let (newAsm, newState) =  generateAsmForExpression x state (getOutputRegister x state) in
        generateAsmForExpressions xs (mergeAsm asm newAsm, newState)
{-

Next Steps :

    - Add Type Declarations
    - Add Boolean as a type + Printing Booleans.
    - Function definitions
    - Custom Types
    - Lists
    - Recursion

-}
generateX86 :: [Expression] -> X86Assembly
generateX86 expressions =
    let oldAsm = getInitialAsm in
    let symbolTable = getNewSymbolTable in
    let state = getInitialState in
    let (newAsm, finalState) = generateAsmForExpressions expressions (getEmptyX86Asm, state) in
    let asmWithDoubles = addDataSection newAsm
                                [DoublesArray doublesArrayConst (getDoubleValues finalState)] in
    mergeMultipleAsm [oldAsm, asmWithDoubles, getEndingAsm finalState]

