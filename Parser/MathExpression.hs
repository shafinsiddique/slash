module Parser.MathExpression where
import Parser.ProgramNode(Expression)
import Parser.Combinator

data MathExpression = AdditionExpr Expression Expression 
                | SubtractionExpr Expression Expression 
                | MultiplicationExpr Expression Expression
                | DivisionExpr Expression Expression


            

