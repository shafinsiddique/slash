module Parser.ExpressionTypes where
    
data ExpressionType = IntType 
    | StrType | DoubleType | BoolType | ErrorType 
    | ListType ExpressionType | CustomType String deriving Show
