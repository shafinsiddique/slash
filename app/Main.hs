module Main where
import Parser.Combinator
import Parser.ExpressionParser ( programParser)
import Parser.StringExpressionParser(stringParser)
import Parser.MathExpressionParser(mathExpressionParser)

import Generator.X86Assembly
    ( X86Assembly(X86Assembly, codeSection, dataSection), mergeAsm )
import Generator.Generator ( generateX86, getInitialAsm, getEndingAsm )
import System.IO
import Data.Foldable
import System.Process
import System.Environment

printAsm :: Maybe X86Assembly -> IO ()
printAsm (Just X86Assembly {codeSection = codeSection, dataSection = dataSection}) =
                                                            mapM_ print (codeSection  ++ dataSection)
printAsm _ = print ""

writeAsmToFile :: Maybe X86Assembly -> IO ()
writeAsmToFile (Just X86Assembly {codeSection = codeSection, dataSection = dataSection}) =
    do
        let asmStr = foldl (\str asm -> str ++ show asm ++ "\n") "" (codeSection ++ dataSection)
        putStrLn asmStr
        writeFile "new.asm" asmStr
        callCommand "nasm -fmacho64 new.asm -o new.o" 
        callCommand "ld new.o std.o -o new -macosx_version_min 11.0 -L /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib -lSystem"
        callCommand "./new"
        callCommand "rm new.asm new.o new"

writeAsmToFile _ = print "There was an error compiling."

main = do
    args <- getArgs
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let ast = runParser programParser contents
    print ast
    let asm = case ast of
            ParsingSuccess nodes _ -> Just (generateX86 nodes)
            _ -> Nothing

    writeAsmToFile asm 
    hClose handle

