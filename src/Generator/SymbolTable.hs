module Generator.SymbolTable where
import Data.Map
import Parser.ExpressionTypes

data VariableInfo = VariableInfo {offset :: Integer, exprType :: ExpressionType}

type SymbolTable = Map String VariableInfo

getExprSize :: ExpressionType -> Integer
getExprSize IntType = 8
getExprSize StrType = 8
getExprSize DoubleType = 8
getExprSize BoolType = 1
getExprSize _ = 8


addSymbolToTable :: SymbolTable -> String -> VariableInfo -> SymbolTable
addSymbolToTable table key value = Data.Map.insert key value table

getVariableSize :: VariableInfo -> Integer
getVariableSize (VariableInfo _ exprType) = getExprSize exprType

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
