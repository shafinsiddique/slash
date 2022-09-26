module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser, printExpressionParser )
import Parser.StringExpressionParser(stringParser)

import Generator.X86Assembly
import Generator.Generator ( generateX86 )

getAsm :: Maybe X86Assembly
getAsm = let parsingResult = runParser printExpressionParser  "println(\"hello\")" in 
    case parsingResult of  
        ParsingSuccess node rest -> Just (generateX86 node)
        ParsingError e -> Nothing
        
main = print getAsm
