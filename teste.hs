module Automat where

import Parser
import Regex
import Data.Foldable

parsingRegex :: String -> [String] -> [RE Char]
parsingRegex exp derivatives
    = case parseRegex exp of
        Right ok -> automatSimulator ok derivatives

automatSimulator :: RE Char -> [String] -> [RE Char]
automatSimulator expression []     = expression:[]
automatSimulator expression (x:[]) = preDeriv expression x:[]
automatSimulator expression (x:xs) = toList (automatSimulator (preDeriv expression x) xs)



preDeriv :: RE Char -> String -> RE Char
preDeriv exp (x:[]) = deriv exp x
preDeriv exp (x:xs) = deriv exp x .*. preDeriv exp xs