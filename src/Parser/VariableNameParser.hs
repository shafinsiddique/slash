module Parser.VariableNameParser (variableNameParser,variableExpressionParser, anyLetterOrNumberParser) where
import Parser.Combinator
import Parser.ProgramNode ( Expression(MathExpr), MathExpression(..) )

-- List String 
anyLetterOrNumberParser :: Parser String
anyLetterOrNumberParser = oneOrMore 
                    (anyOf (map charParser (['a','b'..'z']++['A','B'..'Z']++
                                ['0','1'..'9'])))

variableNameParser :: Parser [Char]
variableNameParser = (\ _ y _ -> y) <$> spaceParser
                    <*> oneOrMore (anyOf (map charParser ('_':['a','b'..'z'])))
                    <*> spaceParser

variableExpressionParser :: Parser Expression
variableExpressionParser = Parser (\input -> case runParser variableNameParser input of
                                    ParsingSuccess name rest -> ParsingSuccess (MathExpr (VariableExpr name)) rest
                                    ParsingError e -> ParsingError e)