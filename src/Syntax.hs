module Syntax 
    where

import Data.Char
import Data.List
import Data.Ord
import Numeric
import Text.ParserCombinators.ReadP
import World

data Toplevel
    = Exps [Term]
    | Proc String [String]

data Value
    = Float Float
    | Bool Bool
    | Cmd [Instruction]
    
data Term
    = ENum Float
    | EBool Bool
    | EVoid
    | EVar String
    | ERepeat Term Term
    | EBlock [Term]
    | EWhen Term Term
    | EIf Term Term Term
    | EProcCall String [Term]
    | EArith Term Bop Term
    | ECompare Term Rop Term
    | EProcDef String [String]
    deriving (Eq, Show)

isAtom :: Term -> Bool
isAtom (ENum _) = True
isAtom (EVar _) = True
isAtom _ = False

-- instance Show Term where
--     show = showTerm

showTerm :: Term -> String
showTerm e = case e of
    ENum x -> show x
    EVar v -> v
    ERepeat e1 e2 -> intercalate " " ["repeat", showTerm e1, showTerm e2]
    EBlock es -> intercalate " " (["begin"] ++ map showTerm es ++ ["end"])
    EWhen e1 e2 -> intercalate " " (["when"] ++ map showTerm [e1, e2])
    EIf e1 e2 e3 -> intercalate " " ["if", showTerm e1, "then", showTerm e2, "else", showTerm e3]
    EProcCall proc vs -> intercalate " " ([proc] ++ map showTerm vs)
    EArith e1 op e2 -> unwords [show' e1, show op, show' e1]
        where
            show' e | isAtom e = show e
                    | otherwise = "(" ++ show e ++ ")"
    EProcDef proc vs -> unwords (proc : vs)

sample1 :: Term
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

instance Read Term where
    readsPrec _ = readP_to_S userInputR

userInputR :: ReadP Term
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
procdefR :: ReadP Term
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
TermR
>>> testR TermR "(3+2) < 3*2"
[(ECompare (EArith (ENum 3.0) + (ENum 2.0)) < (EArith (ENum 3.0) * (ENum 2.0)),"")]
-}
termR :: ReadP Term
termR = compareR +++ arithTermR
{- |
compareR
>>> testR compareR "3+2 < 3*2"
[(ECompare (EArith (ENum 3.0) + (ENum 2.0)) < (EArith (ENum 3.0) * (ENum 2.0)),"")]
-}
compareR :: ReadP Term
compareR = ECompare <$> arithTermR <*> ropR <*> arithTermR
{- |
arithTermR
>>> testR arithTermR "3"
[(ENum 3.0,"")]
-}
arithTermR :: ReadP Term
arithTermR = mkBinOp <$> multiveR <*> many (pair plusR multiveR)
{- |
multiveR
>>> testR multiveR "3"
[(ENum 3.0,"")]
-}
multiveR :: ReadP Term
multiveR = mkBinOp <$> aTermR <*> many (pair multR aTermR)
{- |
aTermR
>>> testR aTermR "3"
[(ENum 3.0,"")]
-}
aTermR :: ReadP Term
aTermR = numR
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
numR :: ReadP Term
numR = ENum <$> (readS_to_P (readSigned readFloat))
{- |
varR
>>> testR varR "  hoge_huga"
[(EVar "hoge","_huga")]
>>> testR varR "hoge2huga"
[(EVar "hoge","2huga")]
-}
varR :: ReadP Term
varR = EVar <$> idenR
{- |[(ERepeat (ENum 4.0) (EProcCall "begin" [EVar "fd",ENum 100.0,EVar "lt",ENum 90.0]),"")]
repeatR
>>> testR repeatR "repeat 4 begin fd(100) lt(90) end"
[]
>>> testR (skipSpaces *> string "repeat" *> arithTermR) "repeat 4"
[(ENum 4.0,"")]
-}
repeatR :: ReadP Term
repeatR = ERepeat <$> (skipSpaces *> string "repeat" *> arithTermR) <*> blockR
{- |
blockR
>>> testR blockR "begin fd(100) end"
hoge
-}
blockR :: ReadP Term
blockR = EBlock <$> between (skipSpaces *> string "begin") (skipSpaces *> string "end") (many1 termR)

condR :: ReadP Term
condR = EIf <$> (skipSpaces *> string "if" *> termR) 
            <*> (skipSpaces *> string "then" *> termR) 
            <*> (skipSpaces *> string "else" *> termR)

outputR :: ReadP Term
outputR = pfail

stopR :: ReadP Term
stopR = pfail

callprocR :: ReadP Term
callprocR = EProcCall <$> idenR <*> many termR

mkBinOp :: Term -> [(Bop,Term)] -> Term
mkBinOp = foldl phi
    where
        phi e (o,e') = EArith e o e'

testR :: ReadP a -> String -> [(a, String)]
testR p s = case readP_to_S p s of 
    [] -> []
    rs -> [minimumBy (comparing (length . snd)) rs]

pair :: ReadP a -> ReadP b -> ReadP (a,b)
pair p q = (,) <$> p <*> q 