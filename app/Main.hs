module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser, printExpressionParser )
import Parser.StringExpressionParser(stringParser)
import Parser.MathExpressionParser(mathExpressionParser)

import Generator.X86Assembly
import Generator.Generator ( generateX86 )

getAsm :: Maybe X86Assembly
getAsm = let parsingResult = runParser mathExpressionParser "2 + 2" in 
    case parsingResult of  
        ParsingSuccess node rest -> Just (generateX86 node RDI)
        ParsingError e -> Nothing
        
main = print getAsm
