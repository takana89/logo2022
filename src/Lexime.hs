module Lexime
    where

import Data.Char
import Numeric

data Lexime
    = ProcId String
    | VarId String
    | Num Double
    | Sym String
    deriving (Eq, Show)

lexer :: String -> [Lexime]
lexer "" = []
lexer ccs@(c:cs)
    | isSpace c  = lexer cs
    | isLetter c = case span isLetter ccs of
        (proc, rs) -> ProcId proc : lexer rs
    | isDigit c  = case readFloat ccs of
        (num, rs):_ -> Num num : lexer rs
    | c == ':'   = case span isLetter cs of
        (var,rs)
            | null var  -> error "lexical error"
            | otherwise -> VarId var : lexer rs
    | c == '-'   = case cs of
        ' ':rs -> Sym "-" : lexer rs
        _      -> case readFloat cs of
            []          -> error "lexical error"
            (num, rs):_ -> Num (negate num) : lexer rs
    | c `elem` ("([+*/,])" :: String) = Sym [c] : lexer cs
    | otherwise = error "lexical error"
