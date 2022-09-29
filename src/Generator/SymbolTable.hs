module Generator.SymbolTable where
import Data.Map

type SymbolTable = Map String Int

addSymbolToTable :: SymbolTable -> String -> Int -> SymbolTable
addSymbolToTable table key value = Data.Map.insert key value table

getNewId :: SymbolTable -> Int 
getNewId table = length (toList table) + 1

getNewSymbolTable :: SymbolTable
getNewSymbolTable = Data.Map.empty 

exists :: String -> SymbolTable -> Bool 
exists = Data.Map.member 

getSize :: SymbolTable -> Integer 
getSize table = toInteger (length (toList table))

findSymbol :: SymbolTable -> String -> Maybe Int 
findSymbol table key = Data.Map.lookup key table 
