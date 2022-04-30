module Parser.ProgramNode 
(
    Expression (..)
) where

-- | A program node is either an expression or a statement. Use this parser to check whether something in the
-- source is an expression. An expression is a mathematical expression, a string, a boolean. Anything that returns
-- something is an expression.
data Expression = IntExpr Integer | FloatExpr Float| Addition Expression Expression 
                | Subtraction Expression Expression
                | Multiplication Expression Expression | Division Expression Expression 
                | StringExpr String
                deriving Show
                

