module Parser.PrintStatementParser 
(
    printStatementParser,
) where

import Parser.Combinator
import Parser.ExpressionParser(expressionParser)
import Parser.ProgramNode(Statement(..), Expression)

handlePrintStatementParser :: String -> Char -> Expression -> Char -> Statement 
handlePrintStatementParser _ _ expr _ = PrintStatement expr

--- TODO : handle spacing.

printStatementParser :: Parser Statement
printStatementParser = handlePrintStatementParser <$> wordParser "println" 
                    <*> charParser '(' 
                    <*> expressionParser 
                    <*> charParser ')' 




