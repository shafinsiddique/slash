module Parser.Expression
(
    expressionParser,

) where

import Parser.Combinator ( Parser, anyOf )
import Parser.ProgramNode (Expression(..))
import Parser.IntegerExpression


 

expressionParser :: Parser Expression
expressionParser = anyOf [integerExpressionParser]
