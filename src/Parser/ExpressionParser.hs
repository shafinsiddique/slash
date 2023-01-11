module Parser.ExpressionParser
(
    expressionParser,
    printExpressionParser,
    letExpressionParser,
    ifExpressionParser,
    programParser

) where

import Parser.Combinator
    ( anyOf,
      charParser,
      optionalParser,
      spaceAndNewlineParser,
      wordParserWithSpaceNewline,
      wordParser,
      runParser,
      oneOrMore,
      wordParserWithSpace,
      Parser(..), zeroOrMore, ParsingResult (ParsingSuccess, ParsingError) )

import Parser.ProgramNode
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser, stringParser1)
import Parser.VariableNameParser(variableNameParser, variableExpressionParser)


handleLetExpression :: String -> String -> Char -> Expression -> String -> Expression -> Expression
handleLetExpression _ varName _ varExp _ = LetExpr varName varExp

-- let x = 2 + 2 in x + 2
letExpressionParser :: Parser Expression
letExpressionParser = handleLetExpression
                <$> wordParserWithSpace "let" -- let
                <*> variableNameParser -- variable name
                <*> charParser '='
                <*> expressionParser
                <*> wordParserWithSpace "in"
                <*> expressionParser

handlePrintStatementParser :: String -> Char -> String -> [Expression] -> Char -> Expression
handlePrintStatementParser _ _ str exprs _ = PrintExpr {toPrint = str, expressions = exprs }

booleanOpParserRight :: BooleanSign -> Parser (BooleanSign, Expression)
booleanOpParserRight sign = (\_ y -> (sign, y)) <$>
                        wordParserWithSpace (show sign)
                        <*> expressionParser

booleanOperationParser :: Parser (BooleanSign, Expression)
booleanOperationParser = anyOf [booleanOpParserRight Equality]

printExpressionParser :: Parser Expression
printExpressionParser = handlePrintStatementParser <$> wordParserWithSpace "println"
                    <*> charParser '('
                    <*> stringParser1
                    <*> zeroOrMore 
                        ((\_ _ x -> x) <$> spaceAndNewlineParser <*> charParser ',' <*> expressionParser)
                    <*> charParser ')'

getBooleanExpression :: Expression -> BooleanSign -> Expression -> Expression
getBooleanExpression left Equality right = BooleanOpExpr (EqualityExpr left right)

handleExpression :: Char -> Expression -> Maybe (BooleanSign, Expression) -> Char -> Expression
handleExpression _ expr maybeBool _ = case maybeBool of
                                    Just (sign, right) -> getBooleanExpression expr sign right
                                    Nothing -> expr

handleIf :: String -> BooleanOp -> String -> Expression -> String -> Expression -> Expression
handleIf _ condition _ thenExp _ = IfExpr condition thenExp

ifExpressionParser :: Parser Expression
ifExpressionParser = handleIf <$> wordParserWithSpace "if"
                    <*> booleanParser 
                    <*> wordParserWithSpace "then"
                    <*> expressionParser
                    <*> wordParserWithSpace "else"
                    <*> expressionParser

trueFalseParser :: Parser BooleanOp 
trueFalseParser = (\_ y _ -> if y == "True" then TrueFalseExpr True else TrueFalseExpr False) 
    <$> spaceAndNewlineParser 
    <*> anyOf [wordParser "True", wordParser "False"] 
    <*> spaceAndNewlineParser

booleanParser :: Parser BooleanOp
booleanParser = anyOf [trueFalseParser]

booleanExpressionParser :: Parser Expression
booleanExpressionParser = Parser 
                    (\input -> case runParser booleanParser input of
                            ParsingSuccess value rest -> ParsingSuccess (BooleanOpExpr value) rest
                            ParsingError e -> ParsingError e)

expressionParser :: Parser Expression
expressionParser = handleExpression <$> spaceAndNewlineParser
                <*> anyOf [stringParser, letExpressionParser, printExpressionParser, ifExpressionParser, mathExpressionParser, 
                 booleanExpressionParser]
                <*> optionalParser booleanOperationParser
                <*> spaceAndNewlineParser

programParser ::  Parser [Expression]
programParser = oneOrMore expressionParser