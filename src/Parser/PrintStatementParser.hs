{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser.PrintStatementParser 
(
    printStatementParser,
) where

import Parser.Combinator ( charParser, Parser, wordParser )
import Parser.StringExpressionParser ( stringParser )
import Parser.ProgramNode ( Expression(PrintExpr, toPrint, StringExpr) )

handlePrintStatementParser :: String -> Char -> Expression -> Char -> Expression 
handlePrintStatementParser _ _ (StringExpr str) _ = PrintExpr {toPrint = str}

--- TODO : handle spacing.

printStatementParser :: Parser Expression
printStatementParser = handlePrintStatementParser <$> wordParser "println" 
                    <*> charParser '(' 
                    <*> stringParser  
                    <*> charParser ')' 




