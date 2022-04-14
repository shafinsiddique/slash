module Main where
import Parser.Combinator
import Parser.Expression ( expressionParser )
main = print (runParser expressionParser "4+4+3/3")
