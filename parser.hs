import Data.Char ( digitToInt, intToDigit )

data ParsingResult a = ParsingSuccess a String | ParsingError String deriving Show

data Parser a = Parser (String -> ParsingResult a)

instance Functor Parser where
    fmap newF (Parser f) =  Parser (\input -> case (f input) of 
                                              ParsingSuccess val rest -> ParsingSuccess (newF val) rest
                                              ParsingError err -> ParsingError err) 

instance Applicative Parser where
    pure a = Parser (\input -> ParsingSuccess a input)
    p1 <*> p2 = Parser (\input -> case (runParser p1 input) of 
                                  ParsingError e -> ParsingError e
                                  ParsingSuccess f rest -> runParser (fmap f p2) rest)

-- The most primitive parser. The Parser returns parsing success if the next character is the expected chr.
charParser :: Char -> Parser Char 
charParser c = Parser (\input -> (case input of
                                 [] -> ParsingError "Expected char, got empty string instead."
                                 (x:xs) -> (if x == c then ParsingSuccess x xs else ParsingError "wrong char.")))

-- Take in a list of Parsers, and return a parser that will take in an input string and keep running
-- the parsers in the list until one of them succeeds. the first one to succeed, the return value is returned.
-- if none matches, return Parsing Error.
anyOf :: [Parser a] -> Parser a
anyOf []  = Parser (\input -> ParsingError "None of the parsers match the given input");
anyOf (x:xs) = Parser (\input -> case (runParser x input) of 
                                 ParsingSuccess val rest -> ParsingSuccess val rest
                                 _ -> (runParser (anyOf xs) input))


digitParser :: Parser Integer 
digitParser = Parser (\input -> case (runParser (anyOf (map charParser (map intToDigit [0,1..9]))) input) of 
                                ParsingSuccess chr rest -> ParsingSuccess (toInteger  (digitToInt chr)) rest
                                ParsingError e -> ParsingError e)


-- return a parser that takes in an input, and keeps matching the passed parser 'p' on the input until it gets an
-- error. This returned parser will never have a ParsingError, if there are no matches, return an empty list.
-- Otherwise, return a list of the parsed values.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = Parser (\input -> case (runParser parser input) of 
                                      ParsingSuccess val rest -> case (runParser (zeroOrMore parser) rest) of 
                                                                 ParsingSuccess l rest -> ParsingSuccess (val:l) rest
                                      ParsingError e -> ParsingSuccess [] input)

-- Same as zeroOrMore but with one restriction, return ParsingError if there's an error.
oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = Parser (\input -> case (runParser (zeroOrMore parser) input) of 
                                     ParsingSuccess [] rest -> ParsingError "No matches, expecting atleast one."
                                     ParsingSuccess l rest -> ParsingSuccess l rest)

listToInteger :: [Integer] -> Integer 
listToInteger [] = 0
listToInteger (x:xs) = x*(10 ^ (length xs)) + (listToInteger xs)

-- A Positive Integer is just one or more of digits.
positiveIntegerParser :: Parser Integer 
positiveIntegerParser = Parser (\input -> case (runParser (oneOrMore digitParser) input) of 
                                          ParsingError e -> ParsingError "No digits matching the input."
                                          ParsingSuccess l rest -> ParsingSuccess (listToInteger l) rest)


handleNegative :: Char -> Integer -> Integer
handleNegative _ num = 0-num

negativeIntegerParser :: Parser Integer 
negativeIntegerParser = pure handleNegative <*> charParser '-' <*> positiveIntegerParser

integerParser :: Parser Integer 
integerParser = anyOf [negativeIntegerParser, positiveIntegerParser]


runParser :: Parser a -> String -> ParsingResult a
runParser (Parser f) input = f input             

optionalParser :: Parser a -> Parser (Maybe a)
optionalParser parser = Parser (\input -> case (runParser parser input) of
                                          ParsingSuccess val rest -> ParsingSuccess (Just val) rest
                                          ParsingError e -> ParsingSuccess Nothing input)

data Expression = IntegerExpression Integer | Addition Expression Expression | Subtraction Expression Expression 
                | Division Expression Expression | Multiplication Expression Expression deriving Show

integerExpressionParser :: Parser Expression
integerExpressionParser = Parser (\input -> case (runParser integerParser input) of
                                            ParsingSuccess val rest -> ParsingSuccess (IntegerExpression val) rest
                                            ParsingError e -> ParsingError e)

parenthesisExpressionParser :: Parser Expression
parenthesisExpressionParser = pure (\_ -> (\y -> (\_ -> y))) <*> charParser '(' <*> expressionParser <*> charParser ')'

expressionStartParser :: Parser Expression
expressionStartParser = anyOf [parenthesisExpressionParser, integerExpressionParser]


handleExpressionEnd :: Char -> Expression -> Expression
handleExpressionEnd _ expr = expr

expressionEndParser :: Char -> Parser Expression
expressionEndParser sign = pure handleExpressionEnd <*> charParser sign <*> expressionParser

handleAddition ::  Expression -> Maybe Expression -> Expression
handleAddition left (Just right) =  Addition left right
handleAddition left Nothing = left

additionParser :: Parser Expression
additionParser = pure handleAddition <*> expressionStartParser <*> optionalParser (expressionEndParser '+')

handleGetExpression :: Char -> Expression -> Expression -> Expression
handleGetExpression sign expr1 expr2 = (getConstructorFromSign sign) expr1 expr2

getExpressionParser :: Char -> Parser Expression
getExpressionParser sign = pure (handleGetExpression sign) <*> expressionStartParser <*> expressionEndParser sign

getConstructorFromSign :: Char -> (Expression -> Expression -> Expression)
getConstructorFromSign '-' = Subtraction
getConstructorFromSign '/' = Division
getConstructorFromSign '+' = Addition
getConstructorFromSign '*' = Multiplication

subtractionParser :: Parser Expression
subtractionParser = getExpressionParser '-'

handleLowerPrecedence :: Char -> Expression -> (Char, Expression)
handleLowerPrecedence sign expr = (sign, expr)

lowerPrecedenceParser :: [Char] -> Parser (Char, Expression)
lowerPrecedenceParser signs = pure handleLowerPrecedence 
                    <*> anyOf (map charParser signs) 
                    <*> expressionParser

handleDivision :: Expression -> Char -> Expression -> Maybe (Char, Expression) -> Expression
handleDivision left _ right Nothing = Division left right
handleDivision left _ right (Just (c, remaining)) = (getConstructorFromSign c) (Division left right) remaining

getDivisionParser :: [Char] -> Parser Expression
getDivisionParser lowerPrecedenceSigns = pure handleDivision 
                <*> expressionStartParser 
                <*> charParser '/' 
                <*> expressionStartParser 
                <*> optionalParser (lowerPrecedenceParser lowerPrecedenceSigns)

divisionParser :: Parser Expression
divisionParser = getDivisionParser ['/','*','+','-']

handleMultiplicationFollowedByDivision :: Expression -> Char -> Expression -> Expression
handleMultiplicationFollowedByDivision left _ right = Multiplication left right 

multiplicationFollowedByDivisionParser :: Parser Expression
multiplicationFollowedByDivisionParser = pure handleMultiplicationFollowedByDivision 
                                    <*> expressionStartParser 
                                    <*> charParser '*' 
                                    <*> getDivisionParser['/']

handleRegularMultiplication :: Expression -> Char -> Expression -> Maybe (Char, Expression) -> Expression
handleRegularMultiplication left _ right Nothing = Multiplication left right
handleRegularMultiplication left _ right (Just (c, remaining)) = (getConstructorFromSign c) (Multiplication left right)
                                                                 remaining

regularMultiplicationParser :: Parser Expression
regularMultiplicationParser = pure handleRegularMultiplication 
                            <*> expressionStartParser 
                            <*> charParser '*'
                            <*> expressionStartParser 
                            <*> (optionalParser (lowerPrecedenceParser ['*','-','+'])) 

multiplicationParser :: Parser Expression
multiplicationParser = anyOf[multiplicationFollowedByDivisionParser, regularMultiplicationParser]

expressionParser :: Parser Expression
expressionParser =  anyOf [divisionParser, multiplicationParser, subtractionParser, additionParser]

-- 2 - 3 / 4 + 3


