module Main where
import Parser.Combinator
import Parser.Expression ( expressionParser )
main = putStrLn (show (runParser expressionParser "1+(2)"))
