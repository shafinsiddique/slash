module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser )
import Parser.StatementParser ( statementParser )
import Parser.PrintStatementParser (printWordParser, printStatementParser)
main = print (runParser printStatementParser "println(\"hello\")")
