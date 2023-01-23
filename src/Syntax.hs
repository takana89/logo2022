module Syntax 
    where

import Data.Char
import Data.List
import Data.Ord
import Numeric
import Text.ParserCombinators.ReadP


data Toplevel
    = Exps [Expr]
    | Proc String [String]

data Value
    = Float Float
    | Bool Bool

type ProcEnv = [(String, Procedure)]
type Procedure = ([String], Expr)
type ValEnv = [(String,Value)]

data Expr
    = ENum Float
    | EBool Bool
    | EVoid
    | EVar String
    | ESeq [Expr]
    | EIf Expr Expr Expr
    | EApp String [Expr]
    | EPlus Expr Expr
    | EMinus Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | ELess Expr Expr
    | EGreater Expr Expr
    deriving (Eq, Show)


{-
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
userInputR =  termR +++ procdefR

sample = unlines sample'
sample' = 
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
>>> testR procdefR (sample' !! 0)
[(EProcDef "tree" ["n"],"")]
-}
procdefR :: ReadP Expr
procdefR = EProcDef <$> (string "proc" *> idenR) <*> many1 idenR
{- |
idenR
>>> testR idenR " hogehoge "
[("hogehoge"," ")]
>>> testR idenR "hoge+huga"
[("hoge","+huga")]
-}
idenR :: ReadP String
idenR = skipSpaces *> munch1 isLetter
{- |
ExprR
>>> testR ExprR "(3+2) < 3*2"
[(ECompare (EArith (ENum 3.0) + (ENum 2.0)) < (EArith (ENum 3.0) * (ENum 2.0)),"")]
-}
termR :: ReadP Expr
termR = compareR +++ arithExprR
{- |
compareR
>>> testR compareR "3+2 < 3*2"
[(ECompare (EArith (ENum 3.0) + (ENum 2.0)) < (EArith (ENum 3.0) * (ENum 2.0)),"")]
-}
compareR :: ReadP Expr
compareR = ECompare <$> arithExprR <*> ropR <*> arithExprR
{- |
arithExprR
>>> testR arithExprR "3"
[(ENum 3.0,"")]
-}
arithExprR :: ReadP Expr
arithExprR = mkBinOp <$> multiveR <*> many (pair plusR multiveR)
{- |
multiveR
>>> testR multiveR "3"
[(ENum 3.0,"")]
-}
multiveR :: ReadP Expr
multiveR = mkBinOp <$> aExprR <*> many (pair multR aExprR)
{- |
aExprR
>>> testR aExprR "3"
[(ENum 3.0,"")]
-}
aExprR :: ReadP Expr
aExprR = numR
        +++ varR
        +++ repeatR
        +++ blockR
        +++ condR
        +++ outputR
        +++ stopR
        +++ between (char '(' ) (char ')') termR
        +++ callprocR
       
{- |
numR
>>> testR numR "3"
[(ENum 3.0,"")]
>>> testR numR " 15.3 "
[(ENum 15.3," ")]
>>> testR numR "35"
[(ENum 35.0,"")]
>>> testR numR "-123.25"
[(ENum (-123.25),"")]
-}
numR :: ReadP Expr
numR = ENum <$> (readS_to_P (readSigned readFloat))
{- |
varR
>>> testR varR "  hoge_huga"
[(EVar "hoge","_huga")]
>>> testR varR "hoge2huga"
[(EVar "hoge","2huga")]
-}
varR :: ReadP Expr
varR = EVar <$> idenR
{- |[(ERepeat (ENum 4.0) (EProcCall "begin" [EVar "fd",ENum 100.0,EVar "lt",ENum 90.0]),"")]
repeatR
>>> testR repeatR "repeat 4 begin fd(100) lt(90) end"
[]
>>> testR (skipSpaces *> string "repeat" *> arithExprR) "repeat 4"
[(ENum 4.0,"")]
-}
repeatR :: ReadP Expr
repeatR = ERepeat <$> (skipSpaces *> string "repeat" *> arithExprR) <*> blockR
{- |
blockR
>>> testR blockR "begin fd(100) end"
hoge
-}
blockR :: ReadP Expr
blockR = EBlock <$> between (skipSpaces *> string "begin") (skipSpaces *> string "end") (many1 termR)

condR :: ReadP Expr
condR = EIf <$> (skipSpaces *> string "if" *> termR) 
            <*> (skipSpaces *> string "then" *> termR) 
            <*> (skipSpaces *> string "else" *> termR)

outputR :: ReadP Expr
outputR = pfail

stopR :: ReadP Expr
stopR = pfail

callprocR :: ReadP Expr
callprocR = EProcCall <$> idenR <*> many termR

mkBinOp :: Expr -> [(Bop,Expr)] -> Expr
mkBinOp = foldl phi
    where
        phi e (o,e') = EArith e o e'

testR :: ReadP a -> String -> [(a, String)]
testR p s = case readP_to_S p s of 
    [] -> []
    rs -> [minimumBy (comparing (length . snd)) rs]

pair :: ReadP a -> ReadP b -> ReadP (a,b)
pair p q = (,) <$> p <*> q 
-}