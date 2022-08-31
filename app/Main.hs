module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser )
import Parser.StatementParser ( statementParser )
import Parser.PrintStatementParser (printStatementParser)
main = print (runParser ifAndParser  "ifand")
