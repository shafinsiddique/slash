module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser )
import Parser.PrintStatementParser (printStatementParser)
import Parser.StringExpressionParser(stringParser)
import Generator.X86Assembly
import Generator.Generator

getAsm :: Maybe X86Assembly
getAsm = let parsingResult = runParser printStatementParser  "println(\"hello\")" in 
    case parsingResult of  
        ParsingSuccess node rest -> Just (generateX86 node)
        ParsingError e -> Nothing
        
main = print getAsm
