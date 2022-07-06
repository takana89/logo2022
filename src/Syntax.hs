module Syntax 
    where

data Expr
    = ENum Double
    | EVar String
    | ERepeat Expr Expr
    | EBlock [Expr]
    | EWhen Expr Expr
    | EIf Expr Expr Expr
    | EProcCall String [Expr]
