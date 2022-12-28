module Generator.ProgramState where
import Generator.X86Assembly (X86Assembly, getEmptyX86Asm)
import Generator.SymbolTable
import Parser.Combinator (charParser)
import Data.Map


data DoublesSection = DoublesSection {values :: [Double], valuesMap :: Map Double Int}

data ProgramState = ProgramState SymbolTable Integer Integer DoublesSection

getInitialDoublesSection :: DoublesSection
getInitialDoublesSection = DoublesSection {values = [], valuesMap = Data.Map.empty}

createNewConst :: ProgramState -> ProgramState
createNewConst (ProgramState table num ifCounter doubles) = ProgramState table (num+1) ifCounter doubles

createNewIf :: ProgramState -> ProgramState
createNewIf (ProgramState table num ifCounter doubles) = ProgramState table num (ifCounter + 1) doubles

getIfCounter :: ProgramState -> Integer
getIfCounter (ProgramState _ _ c _) = c

getConstId :: ProgramState -> Integer
getConstId (ProgramState _ num ifcounter _) = num

getInitialState :: ProgramState
getInitialState = ProgramState getNewSymbolTable 0 0 getInitialDoublesSection

getNewSymbolOffset :: ProgramState -> Int
getNewSymbolOffset (ProgramState table _ _ _) = getNewId table

addSymbol :: ProgramState -> String -> VariableInfo -> ProgramState
addSymbol (ProgramState table num ifCounter doubles) name val = ProgramState
                                            (addSymbolToTable table name val) num ifCounter doubles

addDoubleValue :: DoublesSection -> Double -> DoublesSection
addDoubleValue DoublesSection {values = values , valuesMap = valuesMap} value =
    if Data.Map.member value valuesMap
        then DoublesSection {values = values, valuesMap = valuesMap}
    else
        DoublesSection {values = values ++ [value], valuesMap = Data.Map.insert value (length values) valuesMap}

addDouble :: ProgramState -> Double -> ProgramState
addDouble (ProgramState table num ifCounter doubles) value =
                                        ProgramState table num ifCounter (addDoubleValue doubles value)

findDouble :: ProgramState -> Double -> Int
findDouble (ProgramState _ _ _ DoublesSection {values = _ , valuesMap = valuesMap}) value = 
                                    case Data.Map.lookup value valuesMap of
                                        Just index -> index
                                        Nothing -> -1

getDoubleValues :: ProgramState -> [Double]                                        
getDoubleValues (ProgramState _ _ _ DoublesSection {values = values, valuesMap = _}) = values

getSymbolTableSize :: ProgramState -> Integer
getSymbolTableSize (ProgramState table _ _ _) = getSize table

findVariable :: ProgramState -> String -> Maybe VariableInfo
findVariable (ProgramState table _ _ _) = findSymbol table

