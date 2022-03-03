module Main where
import Parser.Combinator
import Parser.Expression ( expressionParser )
main = print (runParser expressionParser "3 * (2 + 3 * 5)")
