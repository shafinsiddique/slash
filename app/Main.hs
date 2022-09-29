module Main where
import Parser.Combinator
import Parser.ExpressionParser ( expressionParser, printExpressionParser )
import Parser.StringExpressionParser(stringParser)
import Parser.MathExpressionParser(mathExpressionParser)

import Generator.X86Assembly
    ( X86Assembly(X86Assembly, codeSection, dataSection), mergeAsm )
import Generator.Generator ( generateX86, getInitialAsm, getEndingAsm )

getAsm :: Maybe X86Assembly
getAsm = let parsingResult = runParser printExpressionParser "println(2+2+(2*4))" in 
    let initialAsm = getInitialAsm in
    let middle = (case parsingResult of  
                    ParsingSuccess node rest -> Just 
                                (mergeAsm (generateX86 node initialAsm) getEndingAsm)
                    ParsingError e -> Nothing) in 
    middle
        
main = case getAsm of 
    Just X86Assembly {codeSection = codeSection, dataSection = dataSection} -> 
                                                        mapM_ print (codeSection ++ dataSection)
    Nothing -> print ""
