module Syntax 
    where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data Expr
    = ENum Float
    | EVar String
    | ERepeat Expr Expr
    | EBlock [Expr]
    | EWhen Expr Expr
    | EIf Expr Expr Expr
    | EProcCall String [Expr]
    | EArith Bop Expr Expr
    | EProcDef String [String]
    deriving (Eq, Show)

isAtom :: Expr -> Bool
isAtom (ENum _) = True
isAtom (EVar _) = True
isAtom _ = False

-- instance Show Expr where
--     show = showExpr

showExpr :: Expr -> String
showExpr e = case e of
    ENum x -> show x
    EVar v -> v
    ERepeat e1 e2 -> intercalate " " ["repeat", showExpr e1, showExpr e2]
    EBlock es -> intercalate " " (["begin"] ++ map showExpr es ++ ["end"])
    EWhen e1 e2 -> intercalate " " (["when"] ++ map showExpr [e1, e2])
    EIf e1 e2 e3 -> intercalate " " ["if", showExpr e1, "then", showExpr e2, "else", showExpr e3]
    EProcCall proc vs -> intercalate " " ([proc] ++ map showExpr vs)
    EArith op e1 e2 -> unwords [show' e1, show op, show' e1]
        where
            show' e | isAtom e = show e
                    | otherwise = "(" ++ show e ++ ")"
    EProcDef proc vs -> unwords (proc : vs)

sample1 :: Expr
sample1 = ERepeat (ENum 4) (EBlock [EProcCall "fd" [ENum 200], EProcCall "lt" [ENum 90]])

data Bop
    = Add | Sub | Mul | Div deriving (Eq)

instance Show Bop where
    show Add = "+" 
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
--

instance Read Expr where
    readsPrec _ = readP_to_S userInputR

userInputR :: ReadP Expr
userInputR = {- exprR +++ -} procdefR

procdefR :: ReadP Expr
procdefR = EProcDef <$> (string "proc" *> idenR) <*> many1 idenR

idenR :: ReadP String
idenR = skipSpaces *> munch1 isLetter

exprR :: ReadP Expr
exprR = undefined