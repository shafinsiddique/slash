module Parser.StatementParser
(
    statementParser,
    
) where

import Parser.ProgramNode (Statement(..))
import Parser.Combinator (Parser, anyOf)
import Parser.PrintStatementParser (printStatementParser)

statementParser :: Parser Statement
statementParser = anyOf [printStatementParser]
