module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Lexeme.hs", "src/Syntax.hs"]
