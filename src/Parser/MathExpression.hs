module Parser.MathExpression (mathExpressionParser) where
import Parser.ProgramNode(Expression(Addition, Subtraction, Multiplication, Division, EmptyExpr))
import Parser.Combinator
import Parser.IntegerExpression
import Control.Arrow (ArrowChoice(right))
import GHC.IO.Buffer (charSize)
import Control.Concurrent (signalQSemN)

parenthesisExpressionParser :: Parser Expression
parenthesisExpressionParser = pure (\_ -> (\y -> (\_ -> y))) <*> charParser '(' <*> mathExpressionParser <*> charParser ')'

expressionStartParser :: Parser Expression
expressionStartParser = (\_ -> (\y -> (\_ -> y))) <$> spaceParser
                        <*> anyOf [parenthesisExpressionParser, integerExpressionParser]
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

-- rightRecursiveParser :: Expression -> [Char] -> Parser Expression
-- rightRecursiveParser left equalPrecedenceSigns = 
    
-- additionParser2 :: Parser Expression
-- additionParser2 = Parser (\input-> case runParser (binaryExpressionParser '+') input of
--                                 ParsingSuccess val rest -> ParsingSuccess val rest
--                                 ParsingError e -> ParsingError e)

anyBinaryExpressionParser :: [Char] -> Parser Expression 
anyBinaryExpressionParser signs = anyOf (map binaryExpressionParser signs)

getHigherPrecedenceSigns :: Char -> [Char]
getHigherPrecedenceSigns '+' = ['/','*']
getHigherPrecedenceSigns '-' = ['/','*']
getHigherPrecedenceSigns '*' = ['/']
getHigherPrecedenceSigns '/' = []
getHigherPrecedenceSigns _ = []

handleHigherPrecedenceParser2 :: Expression -> Char -> Expression -> Expression
handleHigherPrecedenceParser2 left sign right = (getConstructorFromSign sign) left right 


higherPrecedenceParser2 :: Char -> Parser Expression 
higherPrecedenceParser2 sign = handleHigherPrecedenceParser2 <$> expressionStartParser 
                                <*> charParser sign
                                <*> anyBinaryExpressionParser (getHigherPrecedenceSigns sign)

mathStepOneParser :: Char -> Parser Expression 
mathStepOneParser sign = anyOf [higherPrecedenceParser2 sign, 
                                                binaryExpressionParser sign]

anyRightRecursion :: Expression -> Parser Expression 
anyRightRecursion left = Parser (\input -> 
                                    case runParser 
                                            (anyOf (map (rightRecursiveMathParser left) ['+','-','*','/'])) input of 
                                    ParsingError e -> ParsingSuccess left input
                                    res -> res)
                            

handleRightRecursionStepOne :: Expression -> Char -> Expression -> Expression 
handleRightRecursionStepOne left sign = (getConstructorFromSign sign) left 

rightRecursiveStepOneParser :: Char -> Expression -> Parser Expression 
rightRecursiveStepOneParser sign left = handleRightRecursionStepOne left <$> charParser sign 
                                    <*> anyOf [anyBinaryExpressionParser (getHigherPrecedenceSigns sign),
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