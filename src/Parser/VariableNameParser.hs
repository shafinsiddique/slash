module Parser.VariableNameParser (variableNameParser) where
import Parser.Combinator

variableNameParser :: Parser [Char]
variableNameParser = (\_ -> (\y -> (\_ -> y))) <$> spaceParser 
                    <*> oneOrMore (anyOf (map charParser ('_':['a','b'..'z']))) 
                    <*> spaceParser