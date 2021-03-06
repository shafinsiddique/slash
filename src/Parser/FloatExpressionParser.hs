module Parser.FloatExpressionParser where

import Parser.Combinator

import Parser.IntegerExpressionParser(integerExpressionParser, positiveIntegerParser)
import Parser.ProgramNode (Expression(IntExpr, FloatExpr))

handleFloatParser :: Expression -> Char -> Integer -> Expression 
handleFloatParser (IntExpr left) _ right = FloatExpr (read ((show left) ++ ('.' : (show right))))

floatExpressionParser :: Parser Expression
floatExpressionParser = handleFloatParser <$> integerExpressionParser 
                        <*> charParser '.'
                        <*> positiveIntegerParser 
