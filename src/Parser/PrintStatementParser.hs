module Parser.PrintStatementParser 
(
    printStatementParser, printWordParser
) where

import Parser.Combinator
import Parser.ExpressionParser(expressionParser)
import Parser.ProgramNode(Statement(..), Expression)

handlePrintStatementParser :: String -> Char -> Expression -> Char -> Statement 
handlePrintStatementParser _ _ expr _ = PrintStatement expr

printStatementParser :: Parser Statement
printStatementParser = handlePrintStatementParser <$> wordParser "println" 
                    <*> charParser '(' 
                    <*> expressionParser 
                    <*> charParser ')' 

printWordParser :: Parser String
printWordParser = wordParser "println"
--- TODO : handle spacing.



