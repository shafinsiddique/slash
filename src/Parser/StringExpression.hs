module Parser.StringExpression 
(
    stringParser
) where


import Parser.Combinator
import Parser.ProgramNode (Expression (StringExpr))

handleStringParser :: Char -> [Char] -> Char -> Expression
handleStringParser _ str _ = StringExpr str

stringParser :: Parser Expression
stringParser = handleStringParser <$> charParser '"'
            <*> zeroOrMoreOnCondition (/= '"')
            <*> charParser '"'