module Parser.ReturnType where
import Parser.ProgramNode
data ReturnType = IntReturn | StringReturn | ErrorReturn

getReturnType :: Expression -> ReturnType
getReturnType expr = case expr of 
    IntExpr _ -> IntReturn
    Addition _ _ -> IntReturn
    Subtraction _ _ -> IntReturn
    Multiplication _ _ -> IntReturn
    StringExpr _ -> StringReturn
    _ -> ErrorReturn