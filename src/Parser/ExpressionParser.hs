module Parser.ExpressionParser
(
    expressionParser,
    printExpressionParser

) where

import Parser.Combinator 
import Parser.ProgramNode (Expression(..))
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser)
import Parser.VariableNameParser(variableNameParser)


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

handlePrintStatementParser :: String -> Char -> Expression -> Char -> Expression 
handlePrintStatementParser _ _ expr _ = PrintExpr {toPrint = expr}

--- TODO : handle spacing.

printExpressionParser :: Parser Expression
printExpressionParser = handlePrintStatementParser <$> wordParser "println" 
                    <*> charParser '(' 
                    <*> expressionParser  
                    <*> charParser ')' 

expressionParser :: Parser Expression
expressionParser = (\_ -> (\y -> (\_ -> y))) <$> spaceAndNewlineParser 
                <*> anyOf [stringParser, mathExpressionParser, letExpressionParser, printExpressionParser]
                <*> spaceAndNewlineParser
