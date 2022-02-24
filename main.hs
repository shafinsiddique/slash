module Main where
import Parser.Combinator
main = putStrLn (show (runParser (charParser 'a') "abc"))
