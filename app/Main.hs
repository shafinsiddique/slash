module Main where
import Parser.Combinator
import Parser.Expression ( expressionParser )
main = print (runParser expressionParser "100 - 50 - 2")
