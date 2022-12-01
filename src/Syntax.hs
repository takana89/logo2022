module Syntax 
    where

import Data.List

data Expr
    = ENum Float
    | EVar String
    | ERepeat Expr Expr
    | EBlock [Expr]
    | EWhen Expr Expr
    | EIf Expr Expr Expr
    | EProcCall String [Expr]
    deriving (Eq)

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr e = case e of
    ENum x -> show x
    EVar v -> v
    ERepeat e1 e2 -> intercalate " " ["repeat", showExpr e1, showExpr e2]
    EBlock es -> intercalate " " (["begin"] ++ map showExpr es ++ ["end"])
    EWhen e1 e2 -> intercalate " " (["when"] ++ map showExpr [e1, e2])
    EIf e1 e2 e3 -> intercalate " " ["if", showExpr e1, "then", showExpr e2, "else", showExpr e3]
    EProcCall proc vs -> intercalate " " ([proc] ++ map showExpr vs)

sample1 :: Expr
sample1 = ERepeat (ENum 4) (EBlock [EProcCall "fd" [ENum 200], EProcCall "lt" [ENum 90]])