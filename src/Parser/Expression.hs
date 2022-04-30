module Parser.Expression
(
    expressionParser,

) where

import Parser.Combinator ( Parser, anyOf )
import Parser.ProgramNode (Expression(..))
import Parser.IntegerExpression
import Parser.MathExpression (mathExpressionParser)
import Parser.StringExpression (stringParser)

expressionParser :: Parser Expression
expressionParser = anyOf [stringParser, mathExpressionParser]
