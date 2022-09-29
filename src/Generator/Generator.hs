module Generator.Generator where
import Parser.ProgramNode
import Generator.X86Assembly
import Generator.SymbolTable
import Generator.ProgramState
import Text.Printf
import GHC.Generics (Constructor(conName))
integerFormattedStringConst = "__slash_integer_format"

getInitialAsm :: X86Assembly
getInitialAsm = let codeSection = [TextSection, Global "_main", Default "rel", Extern "_printf",
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

getPrintAsm :: Expression -> Register -> ProgramState -> (X86Assembly, ProgramState)
getPrintAsm expr register state  =
    case expr of
        Addition _ _ -> (printMath expr, state)
        Subtraction _ _ -> (printMath expr, state)
        Multiplication _ _ -> (printMath expr, state)
        Division _ _ -> (printMath expr, state)
        StringExpr str -> printStr str state
        _ -> (getEmptyX86Asm, state)

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

-- getLetExprAsm :: String -> Expression -> Expression -> X86Assembly -> SymbolTable -> Register ->    
--                                                                             (X86Assembly, SymbolTable)
-- getLetExprAsm varName value expr oldAsm symbolTable reg = 
--     let valueAsm = generateAsmForExpression value oldAsm symbolTable R8 in 
--     The value is evaluated and stored in 

-- getLetExprAsm :: String -> Expression -> Expression -> ProgramState -> Register -> ProgramState 
-- getLetExprAsm name value expr state reg = 
--     let oldAsm = getAsm state in 
--     let scratchRegister = R8 in 
--     let symbolTable = getSymbolTable state in
--     let id = getNewId symbolTable in 
--     let evaluateValue = generateAsmForExpression value state scratchRegister in 
--     let symbolTable = addSymbol symbolTable name id in 
--     let addValueToStack = addCodeSection (getAsm evaluateValue) [MOVToMem RBP id scratchRegister] in 

--     -- let valueAsm = generateAsmForExpression value state scratchRegister in 
--     -- let id = getNewId symbolTable in 
--     -- let symbolTable = addSymbol symbolTable name id in -- Add var name to symbol table.
--     -- let addVarToStack = addCodeSection (getAsm valueAsm) [MOVToMem RBP id scratchRegister] in 
--     -- let newState = generateAsmForExpression expr (Generator.ProgramState.newState (getAsm valueAsm)         
--     --                                                 symbolTable) scratchRegister in

getLetExprAsm :: String -> Expression -> Expression -> Register -> ProgramState -> 
                                                                    (X86Assembly, ProgramState)

getLetExprAsm name value expr reg state = 
    let scratchRegister = R8 in 
    let (valAsm, valState) = generateAsmForExpression value state scratchRegister in 
    let symbolOffset = getNewSymbolOffset valState in 
    let stateWithNewSymbol = addSymbol valState name symbolOffset in 
    let addToStackAsm = addCodeSection valAsm [MOVToMem RBP symbolOffset scratchRegister] in 
    let (exprAsm, finalState) = generateAsmForExpression expr stateWithNewSymbol reg in 
    (mergeAsm addToStackAsm exprAsm, finalState)

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

generateX86 :: Expression -> X86Assembly ->  X86Assembly
generateX86 expression oldAsm =
    let symbolTable = getNewSymbolTable in
    let state = getInitialState in
    let (newAsm, newState) = generateAsmForExpression expression state R8 in
    let finalAsm = addStackAsm newAsm newState in 
    mergeAsm oldAsm finalAsm

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
