module Generator.ProgramState where
import Generator.X86Assembly (X86Assembly, getEmptyX86Asm)
import Generator.SymbolTable


data ProgramState = ProgramState SymbolTable Integer

createNewConst :: ProgramState -> ProgramState
createNewConst (ProgramState table num) = ProgramState table (num+1)

getConstId :: ProgramState -> Integer
getConstId (ProgramState _ num) = num

getInitialState :: ProgramState
getInitialState = ProgramState getNewSymbolTable 0

getNewSymbolOffset :: ProgramState -> Int
getNewSymbolOffset (ProgramState table _) = getNewId table

addSymbol :: ProgramState -> String -> VariableInfo -> ProgramState
addSymbol (ProgramState table num) name val = ProgramState (addSymbolToTable table name val) num

getSymbolTableSize :: ProgramState -> Integer
getSymbolTableSize (ProgramState table _) = getSize table

findVariable :: ProgramState -> String -> Maybe VariableInfo 
findVariable (ProgramState table _) = findSymbol table

