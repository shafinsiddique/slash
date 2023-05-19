module Parser.IntegerExpressionParser
(
    integerExpressionParser, positiveIntegerParser
) where

import Parser.ProgramNode (Expression(IntExpr, MathExpr), MathExpression (IntExp))
import Parser.Combinator

listToInteger :: [Integer] -> Integer
listToInteger [] = 0
listToInteger (x:xs) = x*(10 ^ length xs) + listToInteger xs

-- A Positive Integer is just one or more of digits.
positiveIntegerParser :: Parser Integer
positiveIntegerParser = Parser (\input -> case runParser (oneOrMore digitParser) input of
                                          ParsingError e -> ParsingError "No digits matching the input."
                                          ParsingSuccess l rest -> ParsingSuccess (listToInteger l) rest)


handleNegative :: Char -> Integer -> Integer
handleNegative _ = negate

negativeIntegerParser :: Parser Integer
negativeIntegerParser = pure handleNegative <*> charParser '-' <*> positiveIntegerParser

integerParser :: Parser Integer
integerParser = anyOf [negativeIntegerParser, positiveIntegerParser]

integerExpressionParser :: Parser Expression
integerExpressionParser = (\x -> MathExpr (IntExp x)) <$> integerParser



