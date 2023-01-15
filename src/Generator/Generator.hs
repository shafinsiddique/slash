{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
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
import GHC.IO.Handle (NewlineMode(inputNL))
doublesArrayConst = "__slash_doubles_array"


newtype State s a = State (s -> (a,s))

runState :: State s a -> s -> (a,s)
runState (State f) = f

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f state = State (\input ->
        let (updatedVal, updatedState) = runState state input in
                                                (f updatedVal, updatedState))
instance Applicative (State s) where
  pure :: a -> State s a
  pure value = State (\input -> (value, input))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) fState state = State (\initial ->
    let (updatedValue, updatedState) = runState state initial in
        let (f, newState) = runState fState updatedState in (f updatedValue, newState) )

instance Monad (State s) where
    return :: a -> State s a
    return value = State (\s -> (value,s) )

    (>>=) :: State s a -> (a -> State s b) -> State s b
    s1 >>= f = State (\initial -> let
        (value, stateVariable) = runState s1 initial
            in runState (f value) stateVariable)


type Generator = State ProgramState X86Assembly

runGenerator :: State s a -> s -> (a, s)
runGenerator = runState

data MathOperation = AddOp | SubOp | MulOp | DivOp

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf",
                                                                    Extern "_strcmp",
                                                                    StartMain, PUSH (WR RBP),
                                                                    MOVR (WR RBP) (WR RSP)] in
    X86Assembly {dataSection =
        [DataSection], codeSection = codeSection}


getEndingAsm :: ProgramState -> X86Assembly
getEndingAsm state = X86Assembly {dataSection = [], codeSection =
        [ADDI (WR RSP) (getBytesAllocated state), POP (WR RBP), MOV (WR RAX) "0", RET]}

getDataConstName :: Integer -> String
getDataConstName = printf "const_%d"

-- Add a const to the data section
-- move the const to the register.
getStringAsm :: String -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getStringAsm str destination state =
    let newState = createNewConst state in
    let newId = getConstId newState in
    let strName = getDataConstName newId in
    let dataSection = [X86StringConst strName str] in
    let codeSection = [MOV (WR destination) strName] in
    (X86Assembly {dataSection = dataSection, codeSection = codeSection}, newState)

stringExprGenerator :: String -> WordReg -> Generator
stringExprGenerator str destination =
    State (\state ->
        let newState = createNewConst state in
        let newId = getConstId newState in
        let strName = getDataConstName newId in
        let dataSection = [X86StringConst strName str] in
        let codeSection = [MOV (WR destination) strName] in
        (X86Assembly {dataSection = dataSection, codeSection = codeSection}, newState))

getAsmOrAddToRemaining :: Expression -> [WordReg] -> [Expression] ->
            [X86Assembly] -> ProgramState -> [X86Instruction] ->
                ([X86Assembly], [Expression], [WordReg], ProgramState, [X86Instruction])

getAsmOrAddToRemaining expr [] remaining asms state pops = (asms, expr:remaining, [], state, pops)
getAsmOrAddToRemaining expr (reg:remainingReg) remaining asms state pops =
                let regForDoubles = registerIsForDoubles reg in
                let exprSize = 8 in
                let pushInstr = if regForDoubles then PUSHDouble else PUSH in
                let originalBytes = getBytesAllocated state in
                let (newAsm, newState) = generateAsmForExpression expr state reg in
                let bytesAllocated = getBytesAllocated newState in
                let finalState = setBytes newState originalBytes in
                let finalAsm = addCodeSection newAsm [ADDI (WR RSP) (bytesAllocated-originalBytes), pushInstr (WR reg)] in
                let popInstr = if regForDoubles then POPDouble else POP in
                (finalAsm:asms, remaining, remainingReg, addBytes finalState exprSize,
                popInstr (WR reg):pops)

getPrintExprsAsms :: [Expression] -> [Expression] -> [WordReg] -> [WordReg] -> Int ->
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

printArgumentExpressionsGenerator :: [Expression] -> [Expression] -> [WordReg] -> [WordReg] -> Int ->
    [X86Assembly] -> [X86Instruction] -> Integer -> State ProgramState ([X86Assembly], [Expression], Int, [X86Instruction])

printArgumentExpressionsGenerator [] remaining _ _ count asms pops originalBytes =
        State(\state -> ((reverse asms, reverse remaining, count, pops), setBytes state originalBytes))

printArgumentExpressionsGenerator (x:xs) remaining regular doubles count asms pops originalBytes = 
    State (\state -> 
        if expressionHasDouble x state then
        let (newAsms, remainingExprs, remainingDoubleReg, newState, newPops) =
                        getAsmOrAddToRemaining x doubles remaining asms state pops in
        let newCount = count + 1 in
        runState (printArgumentExpressionsGenerator xs remainingExprs regular remainingDoubleReg newCount newAsms newPops originalBytes) newState

    else
        let (newAsms, remainingExprs, remainingReg, newState, newPops) = getAsmOrAddToRemaining x regular remaining asms state pops in
            runState (printArgumentExpressionsGenerator xs remainingExprs remainingReg doubles count newAsms newPops originalBytes) newState
        )


evaluateExprStoreToStack :: Expression -> WordReg -> WordReg ->
    Int -> ProgramState -> (X86Assembly, ProgramState)
evaluateExprStoreToStack expression regularReg doubleReg offset state =
        if expressionHasDouble expression state then
               let (newAsm, newState) = generateAsmForExpression expression state doubleReg in
                (mergeMultipleAsm [newAsm, getX86Assembly [MOVFloatToMem (WR RSP) offset (WR doubleReg) ]], newState)

            else
                let (newAsm, newState) = generateAsmForExpression expression state regularReg in
                (mergeMultipleAsm [newAsm, getX86Assembly [MOVPToMem (WR RSP) offset (WR regularReg)]], newState)


getRemainingExprsToStackAsm :: [Expression] -> WordReg -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getRemainingExprsToStackAsm expressions regularReg doubleReg state =
    let (newAsm, _, newState) = Data.Foldable.foldl (\(asm, offset, state) expr ->
            let (newAsm, newState) = evaluateExprStoreToStack expr regularReg doubleReg offset state in
                (mergeAsm asm newAsm, offset + 1, newState)) (getEmptyX86Asm, 0, state) expressions in
    (newAsm, newState)

remainingExprsToStackGenerator :: [Expression] -> WordReg -> WordReg -> Generator 
remainingExprsToStackGenerator expressions regularReg doubleReg = 
    State (\state -> 
        let (newAsm, _, newState) = Data.Foldable.foldl (\(asm, offset, state) expr -> 
                let (newAsm, newState) = evaluateExprStoreToStack expr regularReg doubleReg offset state in
                (mergeAsm asm newAsm, offset + 1, newState)) (getEmptyX86Asm, 0, state) expressions in
    (newAsm, newState))

getDivRegistersAsm :: Register -> Register -> X86Assembly
getDivRegistersAsm left right = getX86Assembly [MOVR (WR RAX) left, MOVI (WR RDX) 0, DIV right]

getPrintAsm :: String -> [Expression] -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getPrintAsm str expressions register state =
    let (strAsm, newState) = getStringAsm str RDI state in
        let originalBytes = getBytesAllocated state in
        let (exprAsms, remaining, doublesCount, secondState, pops) = getPrintExprsAsms expressions []  [RSI, RDX, RCX, R8, R9] (Prelude.map DoubleReg [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]) 0 [] newState [] originalBytes in
        let regularReg = R8 in
        let doubleReg = DoubleReg XMM0 in
        let (remainingAsm, finalState) =
                        getRemainingExprsToStackAsm remaining regularReg doubleReg secondState in
        let stackSize = toInteger ((((length remaining * 8) `div` 16) + 1) * 16) in
        let stackTopOffset = fromIntegral stackSize `div` 8 in
           (mergeMultipleAsm [strAsm, addCodeSection (mergeMultipleAsm exprAsms) pops,
                        getX86Assembly [MOVR (WR R11) (WR RDX), MOVI (WR R10) 16], getDivRegistersAsm (WR RSP) (WR R10),
                        getX86Assembly [SUB (WR RSP) (WR RDX), SUBI (WR RSP) 32, MOVPToMem (WR RSP) 0 (WR regularReg), MOVFloatToMem (WR RSP) 1 (WR doubleReg), MOVPToMem (WR RSP) 2 (WR RDX), SUBI (WR RSP) stackSize, MOVR (WR RDX) (WR R11)],
        remainingAsm, getX86Assembly [MOVPFromMem (WR regularReg) stackTopOffset (WR RSP), MOVFloatFromMem (WR doubleReg) (stackTopOffset+1) (WR RSP), MOVI (WR RAX) (toInteger doublesCount), CALL "_printf", ADDI (WR RSP) (stackSize + 16), POP (WR RDX), ADDI (WR RSP) 8, ADD (WR RSP) (WR RDX)]], finalState)

-- 
{-
printArgumentExpressionsGenerator [] remaining _ _ count asms pops originalBytes =
        State(\state -> ((reverse asms, reverse remaining, count, pops), setBytes state originalBytes))

-}
-- ([X86Assembly], [Expression], Int, [X86Instruction])
printExprGenerator :: String -> [Expression] -> WordReg -> Generator
printExprGenerator str expressions register = 
    State (\state -> 
        let originalBytes = getBytesAllocated state in 
            let monad = 
                    do {
                        strAsm <- stringExprGenerator str RDI;
                        (exprAsms, remaining, doublesCount, pops) <- 
                        printArgumentExpressionsGenerator expressions [] [RSI, RDX, RCX, R8, R9] (Prelude.map DoubleReg [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]) 0 [] [] originalBytes;
                        let (regularReg, doubleReg) = (R8,DoubleReg XMM0) in 
                            (do {
                                 remainingAsm <- remainingExprsToStackGenerator remaining regularReg doubleReg;
                                 let stackSize = toInteger ((((length remaining * 8) `div` 16) + 1) * 16) in
                                    let stackTopOffset = fromIntegral stackSize `div` 8 in
                                        return  (mergeMultipleAsm [strAsm, addCodeSection (mergeMultipleAsm exprAsms) pops,
                        getX86Assembly [MOVR (WR R11) (WR RDX), MOVI (WR R10) 16], getDivRegistersAsm (WR RSP) (WR R10),
                        getX86Assembly [SUB (WR RSP) (WR RDX), SUBI (WR RSP) 32, MOVPToMem (WR RSP) 0 (WR regularReg), MOVFloatToMem (WR RSP) 1 (WR doubleReg), MOVPToMem (WR RSP) 2 (WR RDX), SUBI (WR RSP) stackSize, MOVR (WR RDX) (WR R11)],
        remainingAsm, getX86Assembly [MOVPFromMem (WR regularReg) stackTopOffset (WR RSP), MOVFloatFromMem (WR doubleReg) (stackTopOffset+1) (WR RSP), MOVI (WR RAX) (toInteger doublesCount), CALL "_printf", ADDI (WR RSP) (stackSize + 16), POP (WR RDX), ADDI (WR RSP) 8, ADD (WR RSP) (WR RDX)]])})

                    } 
            in runState monad state)

registerIsForDoubles :: WordReg -> Bool
registerIsForDoubles (DoubleReg _) = True
registerIsForDoubles _ = False

createAndAddSymbol :: ProgramState -> String -> ExpressionType -> ProgramState
createAndAddSymbol = addSymbol

getIntExprAsm :: Integer -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getIntExprAsm value reg state =
    if registerIsForDoubles reg then getDoubleExprAsm (fromIntegral value) reg state else
    (getX86Assembly [MOVI (WR reg) value], state)

intExprGenerator :: Integer -> WordReg -> Generator
intExprGenerator value reg =
    State (\state -> if registerIsForDoubles reg then getDoubleExprAsm (fromIntegral value) reg state else

                (getX86Assembly [MOVI (WR reg) value], state))

getMathExprDoubleAsm :: Expression -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
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
                        [SUBI (WR RSP) 8, MOVFloatToMem (WR RSP) 0 (WR leftReg)], rightAsm, getX86Assembly [MOVFloatFromMem (WR leftReg) 0 (WR RSP), ADDI (WR RSP) 8, instr (WR leftReg) (WR rightReg), MOVSD (WR reg) (WR leftReg)]], rightState)

doubleMathExprGenerator :: Expression -> Expression -> MathOperation -> WordReg -> Generator
doubleMathExprGenerator left right operator reg =
    let instr = (case operator of
             AddOp -> ADDSD
             SubOp -> SUBSD
             MulOp -> MULSD
             DivOp -> DIVSD) in
    let leftReg = DoubleReg XMM0 in
    let rightReg = DoubleReg XMM1 in
    do {
        leftAsm <- expressionGenerator left leftReg;
        rightAsm <- expressionGenerator right rightReg;
        return (mergeMultipleAsm [leftAsm, getX86Assembly
                        [SUBI (WR RSP) 8, MOVFloatToMem (WR RSP) 0 (WR leftReg)], rightAsm, getX86Assembly [MOVFloatFromMem (WR leftReg) 0 (WR RSP), ADDI (WR RSP) 8, instr (WR leftReg) (WR rightReg), MOVSD (WR reg) (WR leftReg)]])
    }

getMathExprAsm2 :: Expression -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getMathExprAsm2 expr reg state =
    if registerIsForDoubles reg then getMathExprDoubleAsm expr reg state
    else
        (let (left, right, instr) =
                case expr of
                    Addition left right -> (left, right, ADD )
                    Subtraction left right -> (left, right, SUB  )
                    Multiplication left right -> (left, right, IMUL) in
        let leftReg =  R8 in
        let rightReg = R9 in
        let (leftGen, rightGen) = (WR leftReg, WR rightReg) in
        let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
        let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
        (mergeMultipleAsm
                [leftAsm, getX86Assembly [PUSH  leftGen], rightAsm,
                getX86Assembly [POP leftGen, instr leftGen rightGen,
                            MOVR (WR reg) leftGen]], rightState))

mathExprGenerator :: Expression -> Expression -> MathOperation -> WordReg -> Generator
mathExprGenerator left right operator reg =
    if registerIsForDoubles reg then doubleMathExprGenerator left right operator reg else
       let instr = (case operator of
             AddOp -> ADD
             SubOp -> SUB
             MulOp -> IMUL) in
       let leftReg =  R8 in
       let rightReg = R9 in
       let (leftGen, rightGen) = (WR leftReg, WR rightReg) in
       do {
           leftAsm <- expressionGenerator left leftReg;
           rightAsm <- expressionGenerator right rightReg;
           return (mergeMultipleAsm
               [leftAsm, getX86Assembly [PUSH  leftGen], rightAsm,
               getX86Assembly [POP leftGen, instr leftGen rightGen,
                           MOVR (WR reg) leftGen]])
       }

divExprGenerator :: Expression -> Expression -> WordReg -> Generator
divExprGenerator left right reg =
    if registerIsForDoubles reg then doubleMathExprGenerator left right DivOp reg
    else
        let leftReg = R8 in
        let rightReg = R9 in
        do {
            leftAsm <- expressionGenerator left leftReg;
            rightAsm <- expressionGenerator right rightReg;
            let ending = getX86Assembly [POP (WR leftReg), MOVI (WR RDX) 0, MOVR (WR RAX) (WR leftReg),
                                DIV (WR rightReg), MOVR (WR reg) (WR RAX)] in
            return (mergeMultipleAsm [leftAsm, getX86Assembly [PUSH (WR leftReg)], rightAsm, ending])
        }

getDivExprAsm :: Expression -> Expression -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getDivExprAsm left right reg state =
    if registerIsForDoubles reg then getMathExprDoubleAsm (Division left right) reg state
    else
        let leftReg = R8 in
        let rightReg = R9 in
        let (leftAsm, leftState) = generateAsmForExpression left state leftReg in
        let (rightAsm, rightState) = generateAsmForExpression right leftState rightReg in
        let ending = getX86Assembly [POP (WR leftReg), MOVI (WR RDX) 0, MOVR (WR RAX) (WR leftReg),
                DIV (WR rightReg), MOVR (WR reg) (WR RAX)] in
        (mergeMultipleAsm [leftAsm, getX86Assembly [PUSH (WR leftReg)], rightAsm, ending], rightState)

{-
Expr size can not be bigger than max register size. If it is, the pointer should be stored. 

 let (exprAsms, remaining, doublesCount, secondState, pops) = getPrintExprsAsms expressions [] [RSI, RDX, RCX, R8, R9] 

-}

{-
If it's a 1 byte data, but we get a  8 byte register,we need the corresponding single byte portion
of this register. This can be calculated easily. THen we ZERO OUT THE ENTIRE REGISTER (since we're only writing to a specific portion, this is important). Then we move it to the appropriate register. 
-}

getCorrespondingSingleByte :: WordReg -> Register
getCorrespondingSingleByte RAX = SB AL
getCorrespondingSingleByte RDI = SB DIL
getCorrespondingSingleByte RSI = SB SIL
getCorrespondingSingleByte RDX = SB DL
getCorrespondingSingleByte RCX = SB CL
getCorrespondingSingleByte R8 = SB R8B
getCorrespondingSingleByte R9 = SB R9B
getCorrespondingSingleByte R10 = SB R10B
getCorrespondingSingleByte R11 = SB R11B
getCorrespondingSingleByte R12 = SB R12B
getCorrespondingSingleByte R13 = SB R13B
getCorrespondingSingleByte R14 = SB R14B
getCorrespondingSingleByte R15 = SB R15B
getCorrespondingSingleByte RBP = SB BPL
getCorrespondingSingleByte RBX = SB BL
getCorrespondingSingleByte RSP = SB SPL
getCorrespondingSingleByte (DoubleReg val) = WR (DoubleReg val)


getCorrespondingDoubleByte :: WordReg -> Register
getCorrespondingDoubleByte RAX = TB AX
getCorrespondingDoubleByte RDI = TB DI
getCorrespondingDoubleByte RSI = TB SI
getCorrespondingDoubleByte RDX = TB DX
getCorrespondingDoubleByte RCX = TB CX
getCorrespondingDoubleByte R8 = TB R8W
getCorrespondingDoubleByte R9 = TB R9W
getCorrespondingDoubleByte R10 = TB R10W
getCorrespondingDoubleByte R11 = TB R11W
getCorrespondingDoubleByte R12 = TB R12W
getCorrespondingDoubleByte R13 = TB R13W
getCorrespondingDoubleByte R14 = TB R14W
getCorrespondingDoubleByte R15 = TB R15W
getCorrespondingDoubleByte RBP = TB BP
getCorrespondingDoubleByte RBX = TB BX
getCorrespondingDoubleByte RSP = TB SP
getCorrespondingDoubleByte (DoubleReg val) = WR (DoubleReg val)

getCorrespondingFourByte :: WordReg -> Register
getCorrespondingFourByte RAX = FB EAX
getCorrespondingFourByte RDI = FB EDI
getCorrespondingFourByte RSI = FB ESI
getCorrespondingFourByte RDX = FB EDX
getCorrespondingFourByte RCX = FB ECX
getCorrespondingFourByte R8 = FB R8D
getCorrespondingFourByte R9 = FB R9D
getCorrespondingFourByte R10 = FB R10D
getCorrespondingFourByte R11 = FB R11D
getCorrespondingFourByte R12 = FB R12D
getCorrespondingFourByte R13 = FB R13D
getCorrespondingFourByte R14 = FB R14D
getCorrespondingFourByte R15 = FB R15D
getCorrespondingFourByte RBP = FB EBP
getCorrespondingFourByte RBX = FB EBX
getCorrespondingFourByte RSP = FB ESP
getCorrespondingFourByte (DoubleReg val) = WR (DoubleReg val)


getCorrectReg :: WordReg -> Integer -> Register
getCorrectReg reg size
    | size <= 1 = getCorrespondingSingleByte reg
    | size <= 2 = getCorrespondingDoubleByte reg
    | size <= 4 = getCorrespondingFourByte reg
    | otherwise = WR reg

getVariableExprAsm :: String -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getVariableExprAsm name reg state =
    case findVariable state name of
        Just VariableInfo {offset = offset, exprType = exprType} ->
                if registerIsForDoubles reg
                    then (getX86Assembly [MOVDoubleFromStack (WR reg) offset], state)
                else
                    let correctReg = getCorrectReg reg (getExprSize exprType) in
                        (getX86Assembly [XOR (WR reg) (WR reg), MOVFromStack correctReg offset], state)

            -- (getX86Assembly [instr correctReg offset], state)
        Nothing -> (getEmptyX86Asm, state)

variableExprGenerator :: String -> WordReg -> Generator
variableExprGenerator name reg = State (\state ->
    case findVariable state name of
        Just VariableInfo {offset = offset, exprType = exprType} ->
                if registerIsForDoubles reg
                    then (getX86Assembly [MOVDoubleFromStack (WR reg) offset], state)
                else
                    let correctReg = getCorrectReg reg (getExprSize exprType) in
                        (getX86Assembly [XOR (WR reg) (WR reg), MOVFromStack correctReg offset], state)

            -- (getX86Assembly [instr correctReg offset], state)
        Nothing -> (getEmptyX86Asm, state))

getLetExprAsm :: String -> Expression -> Expression -> WordReg -> ProgramState ->
                                                                    (X86Assembly, ProgramState)
getLetExprAsm name value expr reg state =
    let exprType = getExprType value state in
    let scratchRegister = getOutputRegister value state in
    let (valAsm, valState) = generateAsmForExpression value state scratchRegister in
    let symbolOffset = getNewSymbolOffset valState exprType in
    let stateWithNewSymbol = addSymbol valState name exprType in
    let movInstr = if registerIsForDoubles scratchRegister then MOVDoubleToStack else MOVToStack in
    let addToStackAsm = addCodeSection valAsm
                [SUBI (WR RSP) (getExprSize exprType), movInstr symbolOffset (WR scratchRegister)] in
    let (exprAsm, finalState) = generateAsmForExpression expr stateWithNewSymbol reg in
    (mergeAsm addToStackAsm exprAsm, finalState)

getMathEqualityCode :: Register -> Register -> Register -> X86Assembly
getMathEqualityCode leftReg rightReg dest = getX86Assembly [SUB leftReg rightReg, MOVR dest leftReg]

getStrEqualityCode :: Register -> Register -> Register -> X86Assembly
getStrEqualityCode leftReg rightReg dest = getX86Assembly [MOVR (WR RDI) leftReg, MOVR (WR RSI) rightReg,
                                            CALL "_strcmp", MOVR dest (WR RAX)]

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
getTrueFalseExprAsm :: Bool -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getTrueFalseExprAsm val reg state = let intRepresentation = if val then 1 else 0 in
    (getX86Assembly [MOVI (WR reg) intRepresentation], state)

trueFalseExprGenerator :: Bool -> WordReg -> Generator
trueFalseExprGenerator val reg = let intRepresentation = if val then 1 else 0 in
    State (\state -> (getX86Assembly [MOVI (WR reg) intRepresentation], state))

getBooleanExprAsm :: BooleanOp -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getBooleanExprAsm expr reg state =
    case expr of
        TrueFalseExpr val -> getTrueFalseExprAsm val reg state

booleanExprGenerator :: BooleanOp -> WordReg -> Generator
booleanExprGenerator expr reg = case expr of
    TrueFalseExpr val -> trueFalseExprGenerator val reg

getIfExprAsm :: BooleanOp -> Expression -> Expression -> WordReg -> ProgramState ->
                                                                    (X86Assembly , ProgramState)

getIfExprAsm cond thenExp elseExp reg state =
    let scratchReg = R10 in
    let (condAsm, condState) = getBooleanExprAsm cond scratchReg state  in
    let newIf = createNewIf condState in
    let ifId = getIfCounter newIf in
    let ifBranchLabel = printf "if_branch_%d" ifId in
    let elseBranchlabel = printf "else_branch_%d" ifId in
    let continueBranchLabel = printf "continue_%d" ifId in
    let mainSectionAsm = addCodeSection condAsm [CMPRI (WR scratchReg) 1,
                                                           JZ ifBranchLabel, JMP elseBranchlabel] in
    let (ifBranchAsm, ifBranchState) = generateAsmForExpression thenExp newIf reg in
    let (elseBranchAsm, finalState) = generateAsmForExpression elseExp ifBranchState reg in

    let finalIf = mergeMultipleAsm [getX86Assembly [Section ifBranchLabel], ifBranchAsm,
                                                   getX86Assembly [JMP continueBranchLabel]] in

    let finalElse = mergeMultipleAsm [getX86Assembly [Section elseBranchlabel],
                                           elseBranchAsm, getX86Assembly [JMP continueBranchLabel]] in

    (mergeMultipleAsm [mainSectionAsm, finalIf, finalElse, getX86Assembly [Section continueBranchLabel]], finalState )




getDoubleExprAsm :: Double -> WordReg -> ProgramState -> (X86Assembly, ProgramState)
getDoubleExprAsm value reg state =
    let newState = addDouble state value in
    let asm = getX86Assembly [MOVUPS (WR reg) doublesArrayConst (findDouble newState value)] in
    (asm, newState)


doubleExprGenerator :: Double -> WordReg -> Generator
doubleExprGenerator value reg = State (\state ->
        let newState = addDouble state value in
        let asm = getX86Assembly [MOVUPS (WR reg) doublesArrayConst (findDouble newState value)] in
        (asm, newState))


generateAsmForExpression :: Expression -> ProgramState -> WordReg -> (X86Assembly, ProgramState)
generateAsmForExpression expression state register =
    let (newAsm, newState) =
            (case expression of
                PrintExpr {toPrint = value, expressions = exprs} -> getPrintAsm value exprs register state
                IntExpr value -> getIntExprAsm value register state
                Addition _ _ -> getMathExprAsm2 expression register state
                Subtraction _ _ -> getMathExprAsm2 expression register state
                Multiplication _ _ -> getMathExprAsm2 expression register state
                Division left right -> getDivExprAsm left right register state
                StringExpr value ->  getStringAsm value register state
                LetExpr name typeName value expr -> getLetExprAsm name value expr register state
                VariableExpr name -> getVariableExprAsm name register state
                BooleanOpExpr booleanExpr -> getBooleanExprAsm booleanExpr register state
                IfExpr cond thenExp elseExp -> getIfExprAsm cond thenExp elseExp register state
                DoubleExpr value -> getDoubleExprAsm value register state) in
    (newAsm, newState)

expressionGenerator :: Expression -> WordReg -> Generator
expressionGenerator expression register  =
    case expression of
        IntExpr value -> intExprGenerator value register
        Addition left right -> mathExprGenerator left right AddOp register
        Subtraction left right -> mathExprGenerator left right SubOp register
        Division left right -> divExprGenerator left right register
        DoubleExpr value -> doubleExprGenerator value register
        BooleanOpExpr booleanExpr -> booleanExprGenerator booleanExpr register
        StringExpr value -> stringExprGenerator value register
        VariableExpr name -> variableExprGenerator name register
        PrintExpr {toPrint = value, expressions = exprs} -> printExprGenerator value exprs register


getStackAllocationAsm :: Integer -> X86Assembly
getStackAllocationAsm val = addCodeSection getEmptyX86Asm [SUBI (WR RSP) val]

getStackCleanupAsm :: Integer -> X86Assembly
getStackCleanupAsm val = addCodeSection getEmptyX86Asm [ADDI (WR RSP) val]

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

_expressionHasDouble (LetExpr name _ value result) state =
    let newState = createAndAddSymbol state name (getExprType value state) in
        _expressionHasDouble result newState

_expressionHasDouble _ _ = False

expressionHasDouble :: Expression -> ProgramState -> Bool
expressionHasDouble = _expressionHasDouble

getExprType :: Expression -> ProgramState -> ExpressionType
getExprType (LetExpr name _ value result) state =
    let newState = createAndAddSymbol state name (getExprType value state) in
        getExprType result newState
getExprType (VariableExpr name) state = case findVariable state name of
    Just VariableInfo {exprType = exprType} -> exprType
    Nothing -> ErrorType

getExprType (BooleanOpExpr _) state = BoolType

getExprType (StringExpr _) state = StrType

getExprType expr state = if expressionHasDouble expr state then DoubleType else IntType

getOutputRegister :: Expression -> ProgramState -> WordReg
getOutputRegister expr state = if expressionHasDouble expr state then DoubleReg XMM0 else R8


generateAsmForExpressions :: [Expression] -> (X86Assembly, ProgramState) -> (X86Assembly, ProgramState)
generateAsmForExpressions [] (asm, state) = (asm, state)
generateAsmForExpressions (x:xs) (asm, state) =
    let (newAsm, newState) =  runGenerator (expressionGenerator x (getOutputRegister x state)) state in
        generateAsmForExpressions xs (mergeAsm asm newAsm, newState)

generateX86 :: [Expression] -> X86Assembly
generateX86 expressions =
    let oldAsm = getInitialAsm in
    let state = getInitialState in
    let (newAsm, finalState) = generateAsmForExpressions expressions (getEmptyX86Asm, state) in
    let asmWithDoubles = addDataSection newAsm
                                [DoublesArray doublesArrayConst (getDoubleValues finalState)] in
    mergeMultipleAsm [oldAsm, asmWithDoubles, getEndingAsm finalState]

{-

Next Steps :

    - Function definitions
    - Custom Types
    - Lists
    - Recursion

-}