module Parser.MathExpressionParser (mathExpressionParser) where
import Parser.ProgramNode(Expression(Addition, Subtraction, Multiplication, Division))
import Parser.Combinator
import Parser.IntegerExpressionParser
import Parser.FloatExpressionParser
import Control.Arrow (ArrowChoice(right))
import GHC.IO.Buffer (charSize)
import Control.Concurrent (signalQSemN)

parenthesisExpressionParser :: Parser Expression
parenthesisExpressionParser = pure (\_ -> (\y -> (\_ -> y))) <*> charParser '(' <*> mathExpressionParser <*> charParser ')'

expressionStartParser :: Parser Expression
expressionStartParser = (\_ -> (\y -> (\_ -> y))) <$> spaceParser
                        <*> anyOf [parenthesisExpressionParser, floatExpressionParser,                 
                                        integerExpressionParser]
                        <*> spaceParser


getConstructorFromSign :: Char -> (Expression -> Expression -> Expression)
getConstructorFromSign '-' = Subtraction
getConstructorFromSign '/' = Division
getConstructorFromSign '+' = Addition
getConstructorFromSign '*' = Multiplication





getCharParsers :: [Char] -> [Parser Char]
getCharParsers = map charParser

allSignsParser :: Parser Char
allSignsParser = anyOf (map charParser ['+','-','*', '/'])

handleBinaryExpression :: Expression -> Char -> Expression -> Expression
handleBinaryExpression left sign = getConstructorFromSign sign left

binaryExpressionParser :: Char -> Parser Expression
binaryExpressionParser sign = handleBinaryExpression <$> expressionStartParser
                            <*> charParser sign
                            <*> expressionStartParser


anyBinaryExpressionParser :: [Char] -> Parser Expression
anyBinaryExpressionParser signs = anyOf (map binaryExpressionParser signs)

getHigherPrecedenceSigns :: Char -> [Char]
getHigherPrecedenceSigns '+' = ['/','*']
getHigherPrecedenceSigns '-' = ['/','*']
getHigherPrecedenceSigns '*' = ['/']
getHigherPrecedenceSigns '/' = []
getHigherPrecedenceSigns _ = []

_higherPrecedence :: Char -> Parser Expression
_higherPrecedence sign = anyBinaryExpressionParser (getHigherPrecedenceSigns sign)


higherPrecedenceRight :: Char -> Expression -> Parser Expression
higherPrecedenceRight sign left = handleBinaryExpression left
                        <$> anyOf (map charParser (getHigherPrecedenceSigns sign))
                        <*> expressionStartParser

higherPrecedenceRecursive :: Char -> Expression -> Parser Expression 
higherPrecedenceRecursive sign left = Parser 
                                        (\input -> case runParser (higherPrecedenceRight sign left) input of
                                                ParsingSuccess expr rest -> runParser            
                                                        (higherPrecedenceRecursive sign expr) rest
                                                ParsingError e -> ParsingSuccess left input)

higherPrecedenceParser2 :: Char -> Parser Expression
higherPrecedenceParser2 sign = Parser 
                                (\input -> case runParser (_higherPrecedence sign) input of 
                                        ParsingSuccess left rest -> 
                                                runParser (higherPrecedenceRecursive sign left) rest
                                        e -> e)


mathStepOneParser :: Char -> Parser Expression
mathStepOneParser sign = handleBinaryExpression <$> expressionStartParser
                        <*> charParser sign
                        <*> anyOf[higherPrecedenceParser2 sign, expressionStartParser]

anyRightRecursion :: Expression -> Parser Expression
anyRightRecursion left = Parser (\input ->
                                    case runParser
                                            (anyOf (map (rightRecursiveMathParser left) ['+','-','*','/'])) input of
                                    ParsingError e -> ParsingSuccess left input
                                    res -> res)


handleRightRecursionStepOne :: Expression -> Char -> Expression -> Expression
handleRightRecursionStepOne left sign = getConstructorFromSign sign left

rightRecursiveStepOneParser :: Char -> Expression -> Parser Expression
rightRecursiveStepOneParser sign left = handleRightRecursionStepOne left <$> charParser sign
                                    <*> anyOf [higherPrecedenceParser2 sign,
                                                expressionStartParser]

rightRecursiveMathParser :: Expression -> Char -> Parser Expression
rightRecursiveMathParser left sign = Parser (\input -> case runParser
                                                                (rightRecursiveStepOneParser sign left) input of
                                                        ParsingSuccess right rest ->
                                                            runParser (anyRightRecursion right) rest
                                                        ParsingError e -> ParsingError e)

getMathParser :: Char -> Parser Expression
getMathParser sign = Parser (\input -> case runParser (mathStepOneParser sign) input of
                                    ParsingSuccess expr rest -> runParser (anyRightRecursion expr) rest
                                    ParsingError e -> ParsingError e)

additionParser :: Parser Expression
additionParser = getMathParser '+'

subtractionParser :: Parser Expression
subtractionParser = getMathParser '-'

multiplicationParser :: Parser Expression
multiplicationParser = getMathParser '*'

divisionParser :: Parser Expression
divisionParser = getMathParser '/'

mathExpressionParser :: Parser Expression
mathExpressionParser = anyOf [additionParser, subtractionParser, multiplicationParser,
                                                        divisionParser ,expressionStartParser]

-- case to consider:
-- 2 + 8 / 2 / 2 * 2

-- What is the sequence that should be.

-- 2 + (8/2)/2 * 2

-- 2 + ()
