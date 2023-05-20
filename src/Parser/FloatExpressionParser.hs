module Parser.FloatExpressionParser where

import Parser.Combinator

import Parser.IntegerExpressionParser(integerExpressionParser,integerParser, positiveIntegerParser)
import Parser.ProgramNode (Expression(MathExpr), MathExpression(..))

handleFloatParser :: Integer -> Char -> Integer -> Expression 
handleFloatParser left _ right =  MathExpr (DoubleExpr (read ((show left) ++ ('.' : (show right)))))

floatExpressionParser :: Parser Expression
floatExpressionParser = handleFloatParser <$> integerParser 
                        <*> charParser '.'
                        <*> positiveIntegerParser 
