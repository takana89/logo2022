module Syntax 
    where

import Data.Char
import Data.List
import Data.Ord
import Numeric
import Lexeme
import Parser

data Toplevel
    = Exps [Expr]
    | Decl String [String]
    deriving (Eq, Show)

data Value
    = Float Float
    | Bool Bool
    deriving (Eq, Show)

type ProcEnv = [(String, Procedure)]
type Procedure = ([String], Expr)
type ValEnv = [(String,Value)]

data Expr
    = ENum Float
    | EBool Bool
    | EVoid
    | EVar String
    | ESeq [Expr]
    | ERepeat Expr Expr
    | EIf Expr Expr Expr
    | EApp String [Expr]
    | EPlus Expr Expr
    | EMinus Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | ELess Expr Expr
    | EGreater Expr Expr
    deriving (Eq, Show)

---

type LogoParser = Parser Lexeme

---
{-
<userinput> ::= <toplevel>+
<toplevel>  ::= <expr>+ | <procdef>
<procdef>   ::= proc <procId> <varId>*
-}

sampleinput :: String
sampleinput
    = unlines
    [ "proc tree :n"
    , "if :n < 5 then stop"
    , "fd(:n)"
    , "lt(30) tree(:n * 0.7)"
    , "rt(60) tree(:n * 0.7)"
    , "lt(30) bk(:n)"
    , "end"
    ]

userinputP :: LogoParser [Toplevel]
userinputP = many1 toplevelP

toplevelP :: LogoParser Toplevel
toplevelP = expsP +++ procP

{- |
procP
>>> parse procP (lexer "proc foo :x :y")
Proc "foo" ["x","y"]
-}
procP :: LogoParser Toplevel
procP = Decl <$> (token (LxProcId "proc") *> procnameP)
             <*> (map varName <$> munch isVarId)

procnameP :: LogoParser String
procnameP = procName <$> satisfy isProcId

varnameP :: LogoParser String
varnameP = varName <$> satisfy isVarId

expsP :: LogoParser Toplevel
expsP = Exps <$> many1 exprP

procdefP :: LogoParser Toplevel
procdefP = Decl <$> (token (LxProcId "proc") *> procnameP) <*> many varnameP

{-
<userinput>   ::= <toplevel>+

<toplevel>    ::= <expressions> | <procdef>
<procdef>     ::= 'proc' <procid> <varid>*
<expressions> ::= <expr>+ 

<expr>             ::= <additive> <compare-c>
<compare-c>        ::= { '<' | '>' } <additive> 
                     | ε
<additive>         ::= <multiplicative> <additive-c>*
<additive-c>       ::= { '+' | '-' } <multiplicative>
<multiplicative>   ::= <aexpr> <multiplicative-c>*
<multiplicative-c> ::= { '*' | '-' } <aexpr>

<aexpr> ::= '(' <expr> ')' | <number> | <variable> 
          | <repeat-statement> | <block>
          | <conditional-statement> | <output-statement>
          | <stop-statement> | <procedure-call>

<repeat-statement>      ::= 'repeat' <expr> <expr>
<block>                 ::= 'begin' <expr>* 'end'
<conditional-statement> ::= 'if' <expr> 'then' <expr> 'else' <expr>
<output-statement>      ::= 'output' '(' expr ')'
<stop-statement>        ::= 'stop'
<procedure-call>        ::= <procid> '(' ')'
                          | <procid> '(' <arglist>

<arglist>               ::= <expr> ')'
                          | <expr> ',' <arglist>
-}

exprP :: LogoParser Expr
exprP = mkBinOp <$> additiveP <*> comparecP

mkBinOp :: Expr -> [(Lexeme, Expr)] -> Expr
mkBinOp e = foldl phi e
    where
        phi e (o, e1) 
            | isSym o = case symName o of
                "<" -> ELess e e1
                ">" -> EGreater e e1
                "+" -> EPlus e e1
                "-" -> EMinus e e1
                "*" -> EMul e e1
                "/" -> EDiv e e1
                _   -> error "mkBinOp: not binary operator"
            | otherwise = error "mkBinOp: not binary operator"

comparecP :: LogoParser [(Lexeme, Expr)]
comparecP = option [] ((:[]) <$> pair (ltP +++ gtP) additiveP)

ltP :: LogoParser Lexeme
ltP = token (LxSym "<")

gtP :: LogoParser Lexeme
gtP = token (LxSym ">")

additiveP :: LogoParser Expr
additiveP = mkBinOp <$> multiplicativeP <*> additivecP

additivecP :: LogoParser [(Lexeme, Expr)]
additivecP = many (pair (plusP +++ minusP) multiplicativeP)

plusP :: LogoParser Lexeme
plusP = token (LxSym "+")

minusP :: LogoParser Lexeme
minusP = token (LxSym "-")

multiplicativeP :: LogoParser Expr
multiplicativeP = mkBinOp <$> aexprP <*>  multiplicativecP

multiplicativecP :: LogoParser [(Lexeme, Expr)]
multiplicativecP = many (pair (mulP +++ divP) aexprP)

mulP, divP :: LogoParser Lexeme
mulP = token (LxSym "*")
divP = token (LxSym "/")

aexprP :: LogoParser Expr
aexprP = between (token (LxSym "(")) (token (LxSym ")")) exprP
