module Parser.ExpressionParser
(
    expressionParser,

) where

import Parser.Combinator ( Parser, anyOf )
import Parser.ProgramNode (Expression(..))
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser)

expressionParser :: Parser Expression
expressionParser = anyOf [stringParser, mathExpressionParser]
