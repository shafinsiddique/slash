module Parser.VariableNameParser (variableNameParser,variableExpressionParser) where
import Parser.Combinator
import Parser.ProgramNode ( Expression(VariableExpr) )

variableNameParser :: Parser [Char]
variableNameParser = (\ _ y _ -> y) <$> spaceParser
                    <*> oneOrMore (anyOf (map charParser ('_':['a','b'..'z'])))
                    <*> spaceParser

variableExpressionParser :: Parser Expression
variableExpressionParser = Parser (\input -> case runParser variableNameParser input of
                                    ParsingSuccess name rest -> ParsingSuccess (VariableExpr name) rest
                                    ParsingError e -> ParsingError e)