module Main where
    
import Compiler
import Parser
import Text.ParserCombinators.Parsec --hiding (spaces)

cerl exp = case (parse pExp "" exp) of
             Left err ->  show err
             Right val -> erlc val
