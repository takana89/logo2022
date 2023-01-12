module Syntax 
    where

import Data.Char
import Data.List
import Numeric
import Text.ParserCombinators.ReadP

data Expr
    = ENum Float
    | EVar String
    | ERepeat Expr Expr
    | EBlock [Expr]
    | EWhen Expr Expr
    | EIf Expr Expr Expr
    | EProcCall String [Expr]
    | EArith Expr Bop Expr
    | ECompare Expr Rop Expr
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
    EArith e1 op e2 -> unwords [show' e1, show op, show' e1]
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

plusR :: ReadP Bop
plusR = (skipSpaces *> (Add <$ string "+"))
    +++ (skipSpaces *> (Sub <$ string "-"))

multR :: ReadP Bop
multR = (skipSpaces *> (Mul <$ string "*"))
    +++ (skipSpaces *> (Div <$ string "/"))

data Rop
    = Lt | Gt deriving (Eq)

instance Show Rop where
    show Lt = "<"
    show Gt = ">"

ropR :: ReadP Rop
ropR = (skipSpaces *> (Lt <$ string "<"))
    +++ (skipSpaces *> (Gt <$ string ">"))

--

instance Read Expr where
    readsPrec _ = readP_to_S userInputR

userInputR :: ReadP Expr
userInputR =  exprR +++ procdefR

sample = unlines
    ["proc tree n"
    ,"if n < 5 then stop"
    ,"fd(n)"
    ,"lt(30) tree(product(n,0.7))"
    ,"rt(60) tree(n * 0.7)"
    ,"lt(30) bk(n)"
    ,"end"
    ]

{- |
procdefR
>>> read (head (lines sample)) :: Expr
-}
procdefR :: ReadP Expr
procdefR = EProcDef <$> (string "proc" *> idenR) <*> many1 idenR

idenR :: ReadP String
idenR = skipSpaces *> munch1 isLetter

exprR :: ReadP Expr
exprR = ECompare <$> arithexprR <*> ropR <*> arithexprR

arithexprR :: ReadP Expr
arithexprR = EArith <$> multiveR <*> plusR <*> multiveR
            +++ multiveR

multiveR :: ReadP Expr
multiveR = EArith <$> aexprR <*> multR <*> aexprR
            +++ aexprR

aexprR :: ReadP Expr
aexprR = between (char '(' ) (char ')') exprR
        +++ numR
        +++ varR
        +++ repeatR
        +++ blockR
        +++ condR
        +++ outputR
        +++ stopR
        +++ callprocR

numR :: ReadP Expr
numR = ENum <$> (readS_to_P readFloat)

varR :: ReadP Expr
varR = EVar <$> idenR
 
repeatR :: ReadP Expr
repeatR = ERepeat <$> (skipSpaces *> string "repeat" *> exprR) <*> exprR

blockR :: ReadP Expr
blockR = EBlock <$> between (string "begin") (string "end") (many1 exprR)

condR :: ReadP Expr
condR = EIf <$> (skipSpaces *> string "if" *> exprR) 
            <*> (skipSpaces *> string "then" *> exprR) 
            <*> (skipSpaces *> string "else" *> exprR)

outputR :: ReadP Expr
outputR = pfail

stopR :: ReadP Expr
stopR = pfail

callprocR :: ReadP Expr
callprocR = EProcCall <$> idenR <*> many exprR

