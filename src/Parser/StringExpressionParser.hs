module Parser.StringExpressionParser
(
    stringParser,
    stringParser1
) where


import Parser.Combinator
import Parser.ProgramNode (Expression (StringExpr))

handleStringParser :: Char -> [Char] -> Char -> Expression
handleStringParser _ str _ = StringExpr str

handleStringParser1 :: Char -> [Char] -> Char -> String
handleStringParser1 _ str _ = str

stringParser1 :: Parser String
stringParser1 = handleStringParser1 <$> charParser '"'
            <*> zeroOrMoreOnCondition (/= '"')
            <*> charParser '"'

stringParser :: Parser Expression
stringParser = Parser (\input ->
    case runParser stringParser1 input of
            ParsingSuccess val rest -> ParsingSuccess (StringExpr val) rest
            ParsingError e -> ParsingError e)
