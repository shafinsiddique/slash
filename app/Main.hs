module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser, printExpressionParser )
import Parser.StringExpressionParser(stringParser)
import Parser.MathExpressionParser(mathExpressionParser)

import Generator.X86Assembly
    ( X86Assembly(X86Assembly, codeSection) )
import Generator.Generator ( generateX86 )

getAsm :: Maybe X86Assembly
getAsm = let parsingResult = runParser mathExpressionParser "2 + (2*3) + 8 + 9" in 
    case parsingResult of  
        ParsingSuccess node rest -> Just (generateX86 node)
        ParsingError e -> Nothing
        
main = case getAsm of 
    Just X86Assembly {codeSection = codeSection} -> mapM_ print codeSection
    Nothing -> print ""
