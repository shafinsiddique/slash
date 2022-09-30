module Main where
import Parser.Combinator
import Parser.ExpressionParser ( programParser)
import Parser.StringExpressionParser(stringParser)
import Parser.MathExpressionParser(mathExpressionParser)

import Generator.X86Assembly
    ( X86Assembly(X86Assembly, codeSection, dataSection), mergeAsm )
import Generator.Generator ( generateX86, getInitialAsm, getEndingAsm )
import System.IO

-- getAsm :: Maybe X86Assembly
-- getAsm = let parsingResult = runParser ifExpressionParser   "if 2 == 5 then println(\"match\") else println(\"unmatch\")" in 
--     let initialAsm = getInitialAsm in
--     let middle = (case parsingResult of  
--                     ParsingSuccess node rest -> Just 
--                                 (mergeAsm (generateX86 node initialAsm) getEndingAsm)
--                     ParsingError e -> Nothing) in 
--     middle

-- main = case getAsm of 
--     Just X86Assembly {codeSection = codeSection, dataSection = dataSection} -> 
--                                                         mapM_ print (codeSection ++ dataSection)
--     Nothing -> print ""
printAsm :: Maybe X86Assembly -> IO ()
printAsm (Just X86Assembly {codeSection = codeSection, dataSection = dataSection}) = 
                                                            mapM_ print (codeSection  ++ dataSection)
printAsm _ = print ""

main = do
    handle <- openFile "./app/hello_world.sl" ReadMode
    contents <- hGetContents handle
    let ast = runParser programParser contents
    let asm = case ast of
            ParsingSuccess nodes _ -> Just (generateX86 nodes)
            _ -> Nothing
    printAsm asm 
    hClose handle

