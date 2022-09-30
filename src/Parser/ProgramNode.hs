module Parser.ProgramNode 
(
    Expression (..),
    BooleanOp(..),
    BooleanSign(..)
) where

-- | A program node is either an expression or a statement. Use this parser to check whether something in the
-- source is an expression. An expression is a mathematical expression, a string, a boolean. Anything that returns
-- something is an expression.

data BooleanSign = Equality 

instance Show BooleanSign where
    show Equality = "=="

data BooleanOp = EqualityExpr Expression Expression deriving Show 

data Expression = IntExpr Integer | FloatExpr Float| Addition Expression Expression 
                | Subtraction Expression Expression
                | Multiplication Expression Expression | Division Expression Expression 
                | StringExpr String
                | LetExpr String Expression Expression
                | PrintExpr { toPrint :: Expression } 
                | VariableExpr String
                | BooleanOpExpr BooleanOp
                | IfExpr Expression Expression Expression
                deriving Show

                

