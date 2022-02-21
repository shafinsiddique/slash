import Data.Char
import System.Posix (inputTime)

data ParsingResult a = ParsingSuccess a String | ParsingError String deriving Show

data Parser a = Parser (String -> ParsingResult a)

-- The most primitive parser. The Parser returns parsing success if the next character is the expected chr.
charParser :: Char -> Parser Char 
charParser c = Parser (\input -> (case input of
                                 [] -> ParsingError "Expected char, got empty string instead."
                                 (x:xs) -> (if x == c then ParsingSuccess x xs else ParsingError "wrong char.")))

-- Take in a list of Parsers, and return a parser that will take in an input string and keep running
-- the parsers in the list until one of them succeeds. the first one to succeed, the return value is returned.
-- if none matches, return Parsing Error.
anyOf :: [Parser a] -> Parser a
anyOf []  = Parser (\input -> ParsingError "None of the parsers match the given input");
anyOf (x:xs) = Parser (\input -> case (runParser x input) of 
                                 ParsingSuccess val rest -> ParsingSuccess val rest
                                 _ -> (runParser (anyOf xs) input))


digitParser :: Parser Integer 
digitParser = Parser (\input -> case (runParser (anyOf (map charParser (map intToDigit [0,1..9]))) input) of 
                                ParsingSuccess chr rest -> ParsingSuccess (toInteger  (digitToInt chr)) rest
                                ParsingError e -> ParsingError e)


-- return a parser that takes in an input, and keeps matching the passed parser 'p' on the input until it gets an
-- error. This returned parser will never have a ParsingError, if there are no matches, return an empty list.
-- Otherwise, return a list of the parsed values.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = Parser (\input -> case (runParser parser input) of 
                                      ParsingSuccess val rest -> case (runParser (zeroOrMore parser) rest) of 
                                                                 ParsingSuccess l rest -> ParsingSuccess (val:l) rest
                                      ParsingError e -> ParsingSuccess [] input)

-- Same as zeroOrMore but with one restriction, return ParsingError if there's an error.
oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = Parser (\input -> case (runParser (zeroOrMore parser) input) of 
                                     ParsingSuccess [] rest -> ParsingError "No matches, expecting atleast one."
                                     ParsingSuccess l rest -> ParsingSuccess l rest)

listToInteger :: [Integer] -> Integer 
listToInteger [] = 0
listToInteger (x:xs) = x*(10 ^ (length xs)) + (listToInteger xs)

-- A Positive Integer is just one or more of digits.
positiveIntegerParser :: Parser Integer 
positiveIntegerParser = Parser (\input -> case (runParser (oneOrMore digitParser) input) of 
                                          ParsingError e -> ParsingError "No digits matching the input."
                                          ParsingSuccess l rest -> ParsingSuccess (listToInteger l) rest)

runParser :: Parser a -> String -> ParsingResult a
runParser (Parser f) input = f input             

add :: Integer -> Integer -> Integer 
add a b = a + b;

main :: IO ()
main = putStrLn (show (runParser positiveIntegerParser "1249888h"));

