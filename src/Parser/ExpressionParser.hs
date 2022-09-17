module Parser.ExpressionParser
(
    expressionParser,

) where

import Parser.Combinator 
import Parser.ProgramNode (Expression(..))
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser)
import Parser.VariableNameParser(variableNameParser)
import Parser.PrintStatementParser(printStatementParser)


handleLetExpression :: String -> String -> Char -> Expression -> String -> Expression -> Expression
handleLetExpression _ varName _ varExp _ exp = LetExpr {varName = varName, varExpr = varExp, expr = exp} 

-- let x = 2 + 2 in x + 2
letExpressionParser :: Parser Expression
letExpressionParser = handleLetExpression 
                <$> wordParserWithSpace "let" -- let
                <*> variableNameParser -- variable name
                <*> charParser '='
                <*> expressionParser 
                <*> wordParserWithSpace "in" 
                <*> expressionParser

expressionParser :: Parser Expression
expressionParser = (\_ -> (\y -> (\_ -> y))) <$> spaceAndNewlineParser 
                <*> anyOf [stringParser, mathExpressionParser, letExpressionParser, printStatementParser]
                <*> spaceAndNewlineParser
