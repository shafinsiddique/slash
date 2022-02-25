module Parser.MathExpression (mathExpressionParser) where
import Parser.ProgramNode(Expression(Addition, Subtraction, Multiplication, Division))
import Parser.Combinator
import Parser.IntegerExpression

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
            
getConstructorFromSign :: Char -> (Expression -> Expression -> Expression)
getConstructorFromSign '-' = Subtraction
getConstructorFromSign '/' = Division
getConstructorFromSign '+' = Addition
getConstructorFromSign '*' = Multiplication

handleSubtraction :: Expression -> Expression -> Expression 
handleSubtraction left right = Subtraction left right

subtractionParser :: Parser Expression
subtractionParser = pure handleSubtraction <*> expressionStartParser <*> expressionEndParser '-'

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

mathExpressionParser :: Parser Expression 
mathExpressionParser = expressionParser