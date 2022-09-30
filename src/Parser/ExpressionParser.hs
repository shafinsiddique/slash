module Parser.ExpressionParser
(
    expressionParser,
    printExpressionParser,
    letExpressionParser,
    ifExpressionParser

) where

import Parser.Combinator
    ( anyOf,
      charParser,
      optionalParser,
      spaceAndNewlineParser,
      wordParser,
      wordParserWithSpace,
      Parser )

import Parser.ProgramNode
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser)
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

handlePrintStatementParser :: String -> Char -> Expression -> Char -> Expression
handlePrintStatementParser _ _ expr _ = PrintExpr {toPrint = expr}

booleanOpParserRight :: BooleanSign -> Parser (BooleanSign, Expression)
booleanOpParserRight sign = (\_ y -> (sign, y)) <$>
                        wordParserWithSpace (show sign)
                        <*> expressionParser

booleanOperationParser :: Parser (BooleanSign, Expression)
booleanOperationParser = anyOf [booleanOpParserRight Equality]

printExpressionParser :: Parser Expression
printExpressionParser = handlePrintStatementParser <$> wordParser "println"
                    <*> charParser '('
                    <*> expressionParser
                    <*> charParser ')'

getBooleanExpression :: Expression -> BooleanSign -> Expression -> Expression
getBooleanExpression left Equality right = BooleanOpExpr (EqualityExpr left right)

handleExpression :: Char -> Expression -> Maybe (BooleanSign, Expression) -> Char -> Expression
handleExpression _ expr maybeBool _ = case maybeBool of
                                    Just (sign, right) -> getBooleanExpression expr sign right
                                    Nothing -> expr

handleIf :: String -> Expression -> String -> Expression -> String -> Expression -> Expression
handleIf _ condition _ thenExp _ = IfExpr condition thenExp

ifExpressionParser :: Parser Expression
ifExpressionParser = handleIf <$> wordParserWithSpace "if"
                    <*> expressionParser
                    <*> wordParserWithSpace "then"
                    <*> expressionParser
                    <*> wordParserWithSpace "else"
                    <*> expressionParser

expressionParser :: Parser Expression
expressionParser = handleExpression <$> spaceAndNewlineParser
                <*> anyOf [stringParser, mathExpressionParser, letExpressionParser, printExpressionParser,
                ifExpressionParser, variableExpressionParser ]
                <*> optionalParser booleanOperationParser
                <*> spaceAndNewlineParser
