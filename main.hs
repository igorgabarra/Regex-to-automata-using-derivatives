module Main where

import Automat
import Parser
import Regex

main :: IO (RE Char)
main =
  do -- read the input program
     putStrLn "\nHi! This algorithm converts a regular expression to a finite automata."
     putStrLn "Version 1.0"
     putStrLn "Author: Igor Gabarra\n\n"
     putStr "Regular Expression: "
     regex  <- getLine
     putStr "States of the automata: "
     states <- getLine

     putStr "\nThe result is: "
     return (parsingRegex regex (words states))
