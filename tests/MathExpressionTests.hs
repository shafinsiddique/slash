module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Parser.MathExpression(mathExpressionParser)
import Parser.ProgramNode(Expression(..))
import Parser.Combinator(Parser, runParser, ParsingResult(..), maybeParsingResult)

addMaybes :: (Num a) => Maybe a -> Maybe a -> Maybe a
addMaybes (Just l) (Just r) = Just (l + r)
addMaybes _ _ = Nothing


subMaybes :: (Num a) => Maybe a -> Maybe a -> Maybe a
subMaybes (Just l) (Just r) = Just (l - r)
subMaybes _ _ = Nothing


evaluateExpression :: Maybe Expression -> Maybe Integer
evaluateExpression Nothing = Nothing
evaluateExpression (Just (IntExpr val)) = Just val
evaluateExpression (Just (Addition left right)) =  addMaybes 
                                                            (evaluateExpression (Just left))       
                                                            (evaluateExpression (Just right))
evaluateExpression (Just (Subtraction left right)) = subMaybes 
                                                            (evaluateExpression (Just left))       
                                                            (evaluateExpression (Just right))

additionTestLabel :: Integer -> String
additionTestLabel num = "Addition Test " ++ show num

getExprResult :: String -> Maybe Integer
getExprResult input = evaluateExpression(maybeParsingResult (runParser mathExpressionParser input))

a1 = TestLabel (additionTestLabel 1) (TestCase $ assertEqual "" (getExprResult "2 + 2") (Just 4))
a2 = TestLabel (additionTestLabel 2) (TestCase $ assertEqual "" (getExprResult "100+100") (Just 200))
a3 = TestLabel (additionTestLabel 3) (TestCase $ assertEqual "" (getExprResult "100 + 100") (Just 200))
a4 = TestLabel (additionTestLabel 4) (TestCase $ assertEqual "" (getExprResult "100 + 100 + 2") (Just 202))
a5 = TestLabel (additionTestLabel 5) 
            (TestCase $ assertEqual "" (getExprResult "100 + 100 + 3 + 5 + 1 + 291") (Just 500))

additionTestCases = TestList [a1, a2, a3, a4, a5]

-- SUBTRACTION TEST CASES -- 

sTestLabel ::  Integer ->  String
sTestLabel num =  "Subtraction Test " ++ (show num)

s1 = TestLabel (sTestLabel 1) (TestCase $ assertEqual "" (Just 50) (getExprResult "100 - 50") )
s2 = TestLabel (sTestLabel 2) (TestCase $ assertEqual "" (Just 48) (getExprResult "100 - 50 - 2") )
s3 = TestLabel (sTestLabel 3) (TestCase $ assertEqual "" (Just 900) (getExprResult "2000-1000 - 100") )
s4 = TestLabel (sTestLabel 4) (TestCase $ assertEqual "" (Just 600) (getExprResult "900 - 100-100 - 100"))

subtractionTestCases = TestList [s1, s2, s3, s4]


 

tests = hUnitTestToTests $ TestList [additionTestCases, subtractionTestCases]

main = defaultMain tests