module StringExpressionTests where
import Parser.Combinator
import Parser.StringExpression(stringParser)
import Data.Maybe
import Parser.ProgramNode (Expression(StringExpr))
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
testLabel :: Integer -> String 
testLabel n = "String Test " ++ show n 

getStringParsingResult :: String -> Maybe String 
getStringParsingResult input = case (runParser stringParser input) of 
                            ParsingSuccess (StringExpr s) r -> Just s
                            _ -> Nothing

t1 = TestLabel (testLabel 1) 
                        (TestCase $ assertEqual "" (Just "hello") (getStringParsingResult "\"hello\"") )

t2 = TestLabel (testLabel 2) 
                        (TestCase $ assertEqual "" (Nothing) (getStringParsingResult "hello") )


t3 = TestLabel (testLabel 3) 
                        (TestCase $ assertEqual "" (Just "") (getStringParsingResult "\"\"") )


t4 = TestLabel (testLabel 4) 
                        (TestCase $ assertEqual "" (Just "h") (getStringParsingResult "\"h\"") )

t5 = TestLabel (testLabel 4) 
                        (TestCase $ assertEqual "" (Just "\n\n") (getStringParsingResult "\"\n\n\"") )
stringTests = TestList [t1, t2, t3, t4, t5]
