module Parser.FloatExpressionParser where

import Parser.Combinator

import Parser.IntegerExpressionParser(integerExpressionParser, positiveIntegerParser)
import Parser.ProgramNode (Expression(IntExpr, DoubleExpr, MathExpr), MathExpression(DoubleExp))

handleFloatParser :: Expression -> Char -> Integer -> Expression 
handleFloatParser (IntExpr left) _ right =  MathExpr (DoubleExp (read ((show left) ++ ('.' : (show right)))))

floatExpressionParser :: Parser Expression
floatExpressionParser = handleFloatParser <$> integerExpressionParser 
                        <*> charParser '.'
                        <*> positiveIntegerParser 
