module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Parser.MathExpression(mathExpressionParser)
import Parser.ProgramNode(Expression(..))
import Parser.Combinator(Parser, runParser, ParsingResult(..), maybeParsingResult)

addMaybes :: (Integral a) => Maybe a -> Maybe a -> Maybe a
addMaybes (Just l) (Just r) = Just (l + r)
addMaybes _ _ = Nothing


subMaybes :: (Integral a) => Maybe a -> Maybe a -> Maybe a
subMaybes (Just l) (Just r) = Just (l - r)
subMaybes _ _ = Nothing

mulMaybes :: (Integral a) => Maybe a -> Maybe a -> Maybe a
mulMaybes (Just l) (Just r) = Just (l * r)
mulMaybes _ _ = Nothing

divMaybes :: (Integral a) => Maybe a -> Maybe a -> Maybe a
divMaybes (Just l) (Just r) = Just (fromIntegral l `div` fromIntegral r)

evaluateExpression :: Maybe Expression -> Maybe Integer
evaluateExpression Nothing = Nothing
evaluateExpression (Just (IntExpr val)) = Just val
evaluateExpression (Just (Addition left right)) =  addMaybes 
                                                            (evaluateExpression (Just left))       
                                                            (evaluateExpression (Just right))
evaluateExpression (Just (Subtraction left right)) = subMaybes 
                                                            (evaluateExpression (Just left))       
                                                            (evaluateExpression (Just right))
evaluateExpression (Just (Multiplication left right)) = mulMaybes 
                                                            (evaluateExpression (Just left)) 
                                                            (evaluateExpression (Just right))
evaluateExpression (Just (Division left right)) = divMaybes (evaluateExpression (Just left))       
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

a6 = TestLabel (additionTestLabel 6) 
            (TestCase $ assertEqual "" (getExprResult "100 + 200 - 5 + 4 - 3") (Just 296))

a7 = TestLabel (additionTestLabel 7) 
            (TestCase $ assertEqual "" (getExprResult "100 * 4 + 3 + (3 + 4) * 2") (Just 417))

a8 = TestLabel (additionTestLabel 8) 
            (TestCase $ assertEqual "" (getExprResult "2 + 2 + 2 * 3 * 4 - 3") (Just 25))


a9 = TestLabel (additionTestLabel 9) 
            (TestCase $ assertEqual "" (getExprResult "2 + 4 * 3 * 4 * 5 * 6") (Just 1442))
additionTestCases = TestList [a1, a2, a3, a4, a5, a6, a7, a8, a9]

-- SUBTRACTION TEST CASES -- 

sTestLabel ::  Integer ->  String
sTestLabel num =  "Subtraction Test " ++ show num

s1 = TestLabel (sTestLabel 1) (TestCase $ assertEqual "" (Just 50) (getExprResult "100 - 50") )
s2 = TestLabel (sTestLabel 2) (TestCase $ assertEqual "" (Just 48) (getExprResult "100 - 50 - 2") )
s3 = TestLabel (sTestLabel 3) (TestCase $ assertEqual "" (Just 900) (getExprResult "2000-1000 - 100") )
s4 = TestLabel (sTestLabel 4) (TestCase $ assertEqual "" (Just 600) (getExprResult "900 - 100-100 - 100"))
s5 = TestLabel (sTestLabel 5) (TestCase $ assertEqual "" (Just 898) (getExprResult "900 - 100+100+2-4"))
s6 = TestLabel (sTestLabel 6) (TestCase $ assertEqual "" (Just (-7)) (getExprResult "4 - 5 * 3 + 4"))
s7 = TestLabel (sTestLabel 7) (TestCase $ assertEqual "" (Just (4)) (getExprResult "(4-4)+(4)"))
s8 = TestLabel (sTestLabel 8) (TestCase $ assertEqual "" (Just (16)) (getExprResult "(4-4)+(4)*2*2"))
s9 = TestLabel (sTestLabel 9) (TestCase $ assertEqual "" (Just (-38)) (getExprResult "10-4*3*4"))
s10 = TestLabel (sTestLabel 10) (TestCase $ assertEqual "" (Just (30)) 
                                                    (getExprResult "100-10-10-10-10*4"))


subtractionTestCases = TestList [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]

-- MULTIPLICATION TEST CASES
 
mTestLabel ::  Integer ->  String
mTestLabel num =  "Multiplication Test " ++ show num

m1 = TestLabel (mTestLabel 1) (TestCase $ assertEqual "" (Just 36) (getExprResult "2*2*2*2*2+4") )
m2 = TestLabel (mTestLabel 2) (TestCase $ assertEqual "" (Just 20) (getExprResult "2*10-4+4") )
m3 = TestLabel (mTestLabel 3) (TestCase $ assertEqual "" (Just (-20)) (getExprResult "2*-10") )
m4 = TestLabel (mTestLabel 4) (TestCase $ assertEqual "" (Just (9999)) 
                                                            (getExprResult "100*100*1 + 3 - 4") )
m5 = TestLabel (mTestLabel 5) (TestCase $ assertEqual "" (Just (27)) (getExprResult "5*5+2-4+(4)") )


multiplicationTestCases = TestList [m1, m2, m3, m4, m5]


-- Division Test Caes

dTestLabel ::  Integer ->  String
dTestLabel num =  "Division Test " ++ show num

d1 = TestLabel (dTestLabel 1) (TestCase $ assertEqual "" (Just 2) (getExprResult "10/5") )
d2 = TestLabel (dTestLabel 2) (TestCase $ assertEqual "" (Just 11) (getExprResult "10/5*4+3") )

d3 = TestLabel (dTestLabel 3) (TestCase $ assertEqual "" (Just 10) (getExprResult "5+10/2") )

d4 = TestLabel (dTestLabel 4) (TestCase $ assertEqual "" (Just 5) (getExprResult "20/2/2") )

d5 = TestLabel (dTestLabel 5) (TestCase $ assertEqual "" (Just 2) (getExprResult "20/10*2-5+3") )

d6 = TestLabel (dTestLabel 6) (TestCase $ assertEqual "" (Just 2) (getExprResult "11/2 - 3") )

d7 = TestLabel (dTestLabel 7) (TestCase $ assertEqual "" (Just 20) (getExprResult "100/(10/2)") )


d8 = TestLabel (dTestLabel 7) (TestCase $ assertEqual "" (Just 78) (getExprResult "100/(10/2)*4+3-(2+3)"))

divisionTestCases = TestList [d1, d2, d3, d4, d5, d6, d7, d8]

tests = hUnitTestToTests $ TestList [additionTestCases, subtractionTestCases, 
                                    multiplicationTestCases, divisionTestCases]

main = defaultMain tests

-- 4 + 3 / 4 / 5
-- 4

-- 4 - 5 * 3 + 4
