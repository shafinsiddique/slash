module Parser.ExpressionParser
(
    expressionParser,
    printExpressionParser,
    letExpressionParser,
    ifExpressionParser,
    programParser,
    completeTypeNameParser,
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
      Parser(..), zeroOrMore, ParsingResult (ParsingSuccess, ParsingError),
      capitalLetterParser,
      )

import Control.Monad


import Parser.ProgramNode
import Parser.IntegerExpressionParser
import Parser.MathExpressionParser (mathExpressionParser)
import Parser.StringExpressionParser (stringParser, stringParser1)
import Parser.VariableNameParser(variableNameParser, variableExpressionParser, anyLetterOrNumberParser)
import Parser.ExpressionTypes (ExpressionType(..))



handleLetExpression :: String -> String -> String -> ExpressionType -> Char -> Expression -> String -> Expression -> Expression
handleLetExpression _ varName _ typeName _ value _ expr = LetExpr {variableName =
        varName, typeName = typeName, value = value, expression = expr}

-- let x = 2 + 2 in x + 2
letExpressionParser :: Parser Expression
letExpressionParser = handleLetExpression
                <$> wordParserWithSpace "let" -- let
                <*> variableNameParser -- variable name
                <*> wordParser ":"
                <*> completeTypeNameParser
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

-- What's in a typename. List A B C or [List]

completeTypeNameParser :: Parser ExpressionType
completeTypeNameParser = anyOf [listTypeNameParser, typeNameParser]

trim :: String -> String
trim [] = ""
trim (x:xs) = if x == ' ' then (trim xs) else x:xs

typeNameParser :: Parser ExpressionType
typeNameParser = do {
    typeNames <- oneOrMore
    (do {
        _ <- spaceAndNewlineParser;
        firstLetter <- capitalLetterParser;
        remaining <- optionalParser anyLetterOrNumberParser;
        _ <- spaceAndNewlineParser;
        case remaining of
          Nothing -> return [' ', firstLetter]
          Just s -> return (' ':(firstLetter:s))
});
    case trim (concat typeNames) of
        "Integer" -> return IntType
        "String" -> return StrType
        "Bool" -> return BoolType
        "Double" -> return DoubleType
        s -> return (CustomType s)
}

listTypeNameParser :: Parser ExpressionType
listTypeNameParser = do {
    left <- charParser '[';
    _ <- spaceAndNewlineParser;
    typeName <- typeNameParser;
    right <- charParser ']';
    _ <- spaceAndNewlineParser;
    return (ListType typeName)
}
-- function hello_word (name: String, age: Int) : Int = 3;
-- FunctionDefinition (In the register, you put the address.)
-- if in the local stack, otherwise check the argument expressions. 
--  



expressionParser :: Parser Expression
expressionParser = handleExpression <$> spaceAndNewlineParser
                <*> anyOf [stringParser, letExpressionParser, printExpressionParser, ifExpressionParser, mathExpressionParser,
                 booleanExpressionParser]
                <*> optionalParser booleanOperationParser
                <*> spaceAndNewlineParser

programParser ::  Parser [Expression]
programParser = oneOrMore expressionParser