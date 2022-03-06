module Parser.MathExpression (mathExpressionParser) where
import Parser.ProgramNode(Expression(Addition, Subtraction, Multiplication, Division))
import Parser.Combinator
import Parser.IntegerExpression

parenthesisExpressionParser :: Parser Expression
parenthesisExpressionParser = pure (\_ -> (\y -> (\_ -> y))) <*> charParser '(' <*> expressionParser <*> charParser ')'

expressionStartParser :: Parser Expression
expressionStartParser = (\_ -> (\y -> (\_ -> y))) <$> spaceParser
                        <*> anyOf [parenthesisExpressionParser, integerExpressionParser]
                        <*> spaceParser

handleExpressionEnd :: Char -> Expression -> Expression
handleExpressionEnd _ expr = expr

expressionEndParser :: Char -> Parser Expression
expressionEndParser sign = handleExpressionEnd <$> charParser sign <*> expressionParser

handleAddition ::  Expression -> Maybe Expression -> Expression
handleAddition left (Just right) =  Addition left right
handleAddition left Nothing = left

additionParser :: Parser Expression
additionParser = handleAddition <$> expressionStartParser <*> optionalParser (expressionEndParser '+')

getConstructorFromSign :: Char -> (Expression -> Expression -> Expression)
getConstructorFromSign '-' = Subtraction
getConstructorFromSign '/' = Division
getConstructorFromSign '+' = Addition
getConstructorFromSign '*' = Multiplication



handleSubtractionHigher :: Expression -> Char -> Expression -> Expression
handleSubtractionHigher left _ right = Subtraction left right

subtractionWithHigherPrecedence :: Parser Expression
subtractionWithHigherPrecedence = handleSubtractionHigher <$> expressionStartParser
                                <*> charParser '-'
                                <*> anyOf[multiplicationParser, divisionParser]

handleSubtraction :: Expression -> Char -> Expression -> Maybe (Char, Expression) -> Expression
handleSubtraction l _ r Nothing = Subtraction l r
handleSubtraction l _ r (Just (c, remaining)) = getConstructorFromSign c (Subtraction l r) remaining

regularSubtraction :: Parser Expression
regularSubtraction = handleSubtraction <$> expressionStartParser
                    <*> charParser '-'
                    <*> expressionStartParser
                    <*> optionalParser (lowerPrecedenceParser ['-', '+'])

subtractionParser :: Parser Expression
subtractionParser = anyOf[subtractionWithHigherPrecedence, regularSubtraction]

handleLowerPrecedence :: Char -> Expression -> (Char, Expression)
handleLowerPrecedence sign expr = (sign, expr)

lowerPrecedenceParser :: [Char] -> Parser (Char, Expression)
lowerPrecedenceParser signs = handleLowerPrecedence <$> anyOf (map charParser signs)
                    <*> expressionParser

handleDivision :: Expression -> Char -> Expression -> Maybe (Char, Expression) -> Expression
handleDivision left _ right Nothing = Division left right
handleDivision left _ right (Just (c, remaining)) = getConstructorFromSign c (Division left right) remaining

getDivisionParser :: [Char] -> Parser Expression
getDivisionParser lowerPrecedenceSigns = handleDivision <$> expressionStartParser
                <*> charParser '/'
                <*> expressionStartParser
                <*> optionalParser (lowerPrecedenceParser lowerPrecedenceSigns)

divisionParser :: Parser Expression
divisionParser = getDivisionParser ['/','*','+','-']

handleMultiplicationFollowedByDivision :: Expression -> Char -> Expression -> Expression
handleMultiplicationFollowedByDivision left _ right = Multiplication left right

multiplicationFollowedByDivisionParser :: Parser Expression
multiplicationFollowedByDivisionParser = handleMultiplicationFollowedByDivision <$> expressionStartParser
                                    <*> charParser '*'
                                    <*> getDivisionParser['/']

handleRegularMultiplication :: Expression -> Char -> Expression -> Maybe (Char, Expression) -> Expression
handleRegularMultiplication left _ right Nothing = Multiplication left right
handleRegularMultiplication left _ right (Just (c, remaining)) = getConstructorFromSign c
                                                                (Multiplication left right)
                                                                remaining

regularMultiplicationParser :: Parser Expression
regularMultiplicationParser = handleRegularMultiplication <$> expressionStartParser
                            <*> charParser '*'
                            <*> expressionStartParser
                            <*> optionalParser (lowerPrecedenceParser ['*','-','+'])

multiplicationParser :: Parser Expression
multiplicationParser = anyOf[multiplicationFollowedByDivisionParser, regularMultiplicationParser]

expressionParser :: Parser Expression
expressionParser =  anyOf [addParser1, expressionStartParser]

-- | Parser for Math Expressions. A math expression can be any number, or calculation. Eg : 2 + (3*4).
mathExpressionParser :: Parser Expression
mathExpressionParser = expressionParser

handleAdd :: Expression -> Char -> Expression -> Expression
handleAdd l _ r = Addition l r

addParser1 :: Parser Expression 
addParser1 = handleAdd <$> expressionStartParser <*> charParser '+' <*> expressionStartParser

