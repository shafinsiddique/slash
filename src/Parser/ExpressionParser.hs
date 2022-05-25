module Parser.ExpressionParser
(
    expressionParser,

) where

import Parser.Combinator ( Parser, anyOf, spaceAndNewlineParser )
import Parser.ProgramNode (Expression(..))
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser)

expressionParser :: Parser Expression
expressionParser = (\_ -> (\y -> (\_ -> y))) <$> spaceAndNewlineParser 
                <*> anyOf [stringParser, mathExpressionParser]
                <*> spaceAndNewlineParser
