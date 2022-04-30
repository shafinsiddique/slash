module Main where
import MathExpressionTests
import StringExpressionTests
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
tests = hUnitTestToTests $ TestList [mathTests, stringTests]
main = defaultMain tests