module Generator.ProgramState where
import Generator.X86Assembly (X86Assembly, getEmptyX86Asm)
import Generator.SymbolTable
import Parser.Combinator (charParser)


data ProgramState = ProgramState SymbolTable Integer Integer

createNewConst :: ProgramState -> ProgramState
createNewConst (ProgramState table num ifCounter) = ProgramState table (num+1) ifCounter

createNewIf :: ProgramState -> ProgramState 
createNewIf (ProgramState table num ifCounter) = ProgramState table num (ifCounter + 1)

getIfCounter :: ProgramState -> Integer 
getIfCounter (ProgramState _ _ c) = c

getConstId :: ProgramState -> Integer
getConstId (ProgramState _ num ifcounter) = num

getInitialState :: ProgramState
getInitialState = ProgramState getNewSymbolTable 0 0

getNewSymbolOffset :: ProgramState -> Int
getNewSymbolOffset (ProgramState table _ _) = getNewId table

addSymbol :: ProgramState -> String -> VariableInfo -> ProgramState
addSymbol (ProgramState table num ifCounter) name val = ProgramState 
                                            (addSymbolToTable table name val) num ifCounter

getSymbolTableSize :: ProgramState -> Integer
getSymbolTableSize (ProgramState table _ _) = getSize table

findVariable :: ProgramState -> String -> Maybe VariableInfo 
findVariable (ProgramState table _ _) = findSymbol table

