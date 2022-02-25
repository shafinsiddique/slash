module Parser.Combinator 
(
    Parser(Parser),
    ParsingResult(ParsingSuccess, ParsingError),
    charParser, 
    anyOf,
    zeroOrMore,
    oneOrMore,
    runParser,
    optionalParser,
    convertParser,
    digitParser
) where

import Data.Char ( digitToInt, intToDigit )

data ParsingResult a = ParsingSuccess a String | ParsingError String deriving Show

-- | The Parser data type is a function that takes in a String and returns a ParsingResult. 
data Parser a = Parser (String -> ParsingResult a)

instance Functor Parser where
    fmap newF (Parser f) =  Parser (\input -> case (f input) of 
                                              ParsingSuccess val rest -> ParsingSuccess (newF val) rest
                                              ParsingError err -> ParsingError err) 

instance Applicative Parser where
    pure a = Parser (\input -> ParsingSuccess a input)
    p1 <*> p2 = Parser (\input -> case (runParser p1 input) of 
                                  ParsingError e -> ParsingError e
                                  ParsingSuccess f rest -> runParser (fmap f p2) rest)

-- | Take in a character 'c' and return our most primitive parser. The returned parser is a function that takes in an input and returns a ParsingSuccess if the first character in the input is the character 'c'.
-- Todo : Fix error messaging.
charParser :: Char -> Parser Char 
charParser c = Parser (\input -> (case input of
                                 [] -> ParsingError "Expected char, got empty string instead."
                                 (x:xs) -> (if x == c then ParsingSuccess x xs else ParsingError "wrong char.")))

-- | Take in a list of Parsers, and return a parser that will take in an input string and keep running
-- the parsers in the list until one of them succeeds. the first one to succeed, the return value is returned.
-- if none matches, return Parsing Error.
anyOf :: [Parser a] -> Parser a
anyOf []  = Parser (\input -> ParsingError "None of the parsers match the given input");
anyOf (x:xs) = Parser (\input -> case (runParser x input) of 
                                 ParsingSuccess val rest -> ParsingSuccess val rest
                                 _ -> (runParser (anyOf xs) input))

-- | Parser to parse into integer if the next character is a digit.
digitParser :: Parser Integer 
digitParser = Parser (\input -> case (runParser (anyOf (map charParser (map intToDigit [0,1..9]))) input) of 
                                ParsingSuccess chr rest -> ParsingSuccess (toInteger  (digitToInt chr)) rest
                                ParsingError e -> ParsingError e)


-- | return a parser that takes in an input, and keeps matching the passed parser 'p' on the input until it gets an
-- error. This returned parser will never have a ParsingError, if there are no matches, return an empty list.
-- Otherwise, return a list of the parsed values. This parser never returns a ParsingError.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = Parser (\input -> case (runParser parser input) of 
                                      ParsingSuccess val rest -> case (runParser (zeroOrMore parser) rest) of 
                                                                 ParsingSuccess l rest -> ParsingSuccess (val:l) rest
                                      ParsingError e -> ParsingSuccess [] input)

-- | Same as zeroOrMore but with one restriction, return ParsingError if there is not atleast one match. 
oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = Parser (\input -> case (runParser (zeroOrMore parser) input) of 
                                     ParsingSuccess [] rest -> ParsingError "No matches, expecting atleast one."
                                     ParsingSuccess l rest -> ParsingSuccess l rest)

-- | A helper function to help run any parser. 
runParser :: Parser a -> String -> ParsingResult a
runParser (Parser f) input = f input             

-- | A parser that runs another parser on the input. If the return value is an error, return Parsing Success with
-- Nothing. Otherwise return the returned value with Just. We use this parser because sometimes a value is optional
-- and we don't want a ParsingError if there was no match.
optionalParser :: Parser a -> Parser (Maybe a)
optionalParser parser = Parser (\input -> case (runParser parser input) of
                                          ParsingSuccess val rest -> ParsingSuccess (Just val) rest
                                          ParsingError e -> ParsingSuccess Nothing input)

-- | Take in a parser that returns a value of type 'a', and a function that converts that value to another type 'b'
-- and return a parser of type 'b'
convertParser :: Parser a -> (a->b) -> Parser b
convertParser parser f = Parser (\input -> case (runParser parser input) of 
                                        ParsingSuccess val rest -> ParsingSuccess (f val) rest
                                        ParsingError e -> ParsingError e)