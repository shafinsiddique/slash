module Parser.ProgramNode 
(
    Expression (..),
    BooleanOp(..),
    BooleanSign(..),
    MathExpression(..),
) where

-- | A program node is either an expression or a statement. Use this parser to check whether something in the
-- source is an expression. An expression is a mathematical expression, a string, a boolean. Anything that returns
-- something is an expression.

data BooleanSign = Equality 

instance Show BooleanSign where
    show Equality = "=="


data BooleanOp = EqualityExpr Expression Expression | TrueFalseExpr Bool deriving Show

data MathExpression = Addition Expression Expression 
    | Subtraction Expression Expression 
    | Division Expression Expression
    | Multiplication Expression Expression
    | IntExpr Integer
    | DoubleExpr Double 
    | VariableExpr String
    deriving Show

data Expression = StringExpr String
                | LetExpr {variableName :: String, typeName :: String,  value::Expression, expression::Expression}
                | PrintExpr { toPrint :: String, expressions :: [Expression] } 
                | BooleanOpExpr BooleanOp
                | IfExpr BooleanOp Expression Expression
                | MathExpr MathExpression 
                deriving Show

                

