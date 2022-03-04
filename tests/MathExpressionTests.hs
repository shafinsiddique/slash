module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

testfoo = TestCase $ assertEqual "SimpleAdditionTest" "bar" "bar"

testfoo2 = TestCase $ assertEqual "Foo != Bar"
    "Foo" "f"

testAdd2 = TestLabel "Addition Test Isolated" (TestCase $ assertEqual "" 2 1)
tests = hUnitTestToTests $ TestList [TestLabel "Simple Addition Test" testfoo, TestLabel "Simple Addition Test2" testfoo2, testAdd2]

main = defaultMain tests