module Lexeme
    ( Lexeme (..)
    , lexer
    , isProcId
    , isVarId
    , isNum
    , isSym
    , procName
    , varName
    , numValue
    , symName
    ) where

import Data.Char
import Numeric

data Lexeme
    = LxProcId String
    | LxVarId String
    | LxNum Float
    | LxSym String
    deriving (Eq, Show)

procName :: Lexeme -> String
procName lx = case lx of
    LxProcId name -> name
    _             -> error "procName: not ProcId"

varName :: Lexeme -> String
varName lx = case lx of
    LxVarId name -> name
    _            -> error "varName: not varId"

numValue :: Lexeme -> Float
numValue lx = case lx of
    LxNum val -> val
    _         -> error "numValue: not Num"

symName :: Lexeme -> String
symName lx = case lx of
    LxSym name -> name
    _            -> error "symName: not Sym"

isProcId, isVarId, isNum, isSym :: Lexeme -> Bool
isProcId lx = case lx of
    LxProcId _ -> True
    _          -> False
isVarId lx = case lx of
    LxVarId _ -> True
    _         -> False
isNum lx = case lx of
    LxNum _ -> True
    _      -> False
isSym lx = case lx of
    LxSym _ -> True
    _       -> False

{- | 字句解析器
>>> lexer "fd(3+:a)"
[LxProcId "fd",LxSym "(",LxNum 3.0,LxSym "+",LxVarId "a",LxSym ")"]
-}
lexer :: String -> [Lexeme]
lexer "" = []
lexer ccs@(c:cs)
    | isSpace c  = lexer cs
    | isLetter c = case span isLetter ccs of
        (procid, rs) -> LxProcId procid : lexer rs
    | isDigit c  = case readFloat ccs of
        (num, rs):_ -> LxNum num : lexer rs
    | c == ':'   = case span isLetter cs of
        (var,rs)
            | null var  -> error "lexer: lexical error"
            | otherwise -> LxVarId var : lexer rs
    | c == '-'   = case cs of
        ' ':rs -> LxSym "-" : lexer rs
        _      -> case readFloat cs of
            []          -> error "lexer: lexical error"
            (num, rs):_ -> LxNum (negate num) : lexer rs
    | c `elem` ("([+*/,])" :: String) = LxSym [c] : lexer cs
    | otherwise = error "lexer: lexical error"
