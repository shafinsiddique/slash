module Generator.ProgramState where
import Generator.X86Assembly (X86Assembly, getEmptyX86Asm, Register (..))
import Generator.SymbolTable
import Parser.Combinator (charParser)
import Data.Map

data DoublesSection = DoublesSection {values :: [Double], valuesMap :: Map Double Int}

data ProgramState = ProgramState {symbolTable :: SymbolTable, constCounter :: Integer,
    ifCounter ::  Integer, doublesSection ::  DoublesSection, bytesAllocated :: Integer}

getInitialDoublesSection :: DoublesSection
getInitialDoublesSection = DoublesSection {values = [], valuesMap = Data.Map.empty}


createNewConst :: ProgramState -> ProgramState
createNewConst (ProgramState table num ifCounter doubles bytes) =
             ProgramState table (num+1) ifCounter doubles bytes

createNewIf :: ProgramState -> ProgramState
createNewIf (ProgramState table num ifCounter doubles bytes) =
        ProgramState table num (ifCounter + 1) doubles bytes

getIfCounter :: ProgramState -> Integer
getIfCounter (ProgramState _ _ c _ _) = c

getConstId :: ProgramState -> Integer
getConstId (ProgramState _ num ifcounter _ _) = num

getInitialState :: ProgramState
getInitialState = ProgramState getNewSymbolTable 0 0 getInitialDoublesSection 0

getNewSymbolOffset :: ProgramState -> ExpressionType -> Integer
getNewSymbolOffset ProgramState {bytesAllocated = bytes} exprType = bytes + getExprSize exprType

addSymbol :: ProgramState -> String -> ExpressionType -> ProgramState
addSymbol (ProgramState table num ifCounter doubles bytes) name exprType =
    let totalBytesAllocated = bytes + getExprSize exprType in
        ProgramState (addSymbolToTable table name (VariableInfo totalBytesAllocated exprType)) num ifCounter doubles totalBytesAllocated


addDoubleValue :: DoublesSection -> Double -> DoublesSection
addDoubleValue DoublesSection {values = values , valuesMap = valuesMap} value =
    if Data.Map.member value valuesMap
        then DoublesSection {values = values, valuesMap = valuesMap}
    else
        DoublesSection {values = values ++ [value], valuesMap = Data.Map.insert value (length values) valuesMap}

addDouble :: ProgramState -> Double -> ProgramState
addDouble (ProgramState table num ifCounter doubles bytes) value =
                                        ProgramState table num ifCounter (addDoubleValue doubles value) bytes

findDouble :: ProgramState -> Double -> Int
findDouble (ProgramState _ _ _ DoublesSection {values = _ , valuesMap = valuesMap} bytes) value =
                                    case Data.Map.lookup value valuesMap of
                                        Just index -> index
                                        Nothing -> -1

getDoubleValues :: ProgramState -> [Double]
getDoubleValues (ProgramState _ _ _ DoublesSection {values = values, valuesMap = _} bytes) = values

getSymbolTableSize :: ProgramState -> Integer
getSymbolTableSize (ProgramState table _ _ _ _) = getSize table

findVariable :: ProgramState -> String -> Maybe VariableInfo
findVariable (ProgramState table _ _ _ _ ) = findSymbol table

getBytesAllocated :: ProgramState -> Integer
getBytesAllocated ProgramState {bytesAllocated = bytesAllocated} = bytesAllocated

addBytes :: ProgramState -> Integer -> ProgramState
addBytes (ProgramState table num ifCounter doubles bytes) newBytes =
                            ProgramState table num ifCounter doubles (bytes+newBytes)

setBytes :: ProgramState -> Integer -> ProgramState
setBytes (ProgramState table num ifCounter doubles _) = ProgramState table num ifCounter doubles
