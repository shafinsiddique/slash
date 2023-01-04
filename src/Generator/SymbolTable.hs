module Generator.SymbolTable where
import Data.Map
import Parser.ReturnType

data VariableInfo = VariableInfo {offset :: Int, isDouble :: Bool}

type SymbolTable = Map String VariableInfo

addSymbolToTable :: SymbolTable -> String -> VariableInfo -> SymbolTable
addSymbolToTable table key value = Data.Map.insert key value table

getNewId :: SymbolTable -> Int 
getNewId table = length (toList table) + 1

getNewSymbolTable :: SymbolTable
getNewSymbolTable = Data.Map.empty 

exists :: String -> SymbolTable -> Bool 
exists = Data.Map.member 

getSize :: SymbolTable -> Integer 
getSize table = toInteger (length (toList table))

findSymbol :: SymbolTable -> String -> Maybe VariableInfo 
findSymbol table key = Data.Map.lookup key table 
