{-# LANGUAGE NPlusKPatterns #-}
module Tree where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Environment
import Turtle

type Order = Int

data L = F | B | M | P deriving Eq

instance Show L where
    show F = "F"
    show B = "B"
    show M = "-"
    show P = "+"

type LSystem = [(L,Order)] -> [(L,Order)]

_F, _B, _P, _M :: Order -> LSystem
_F o = ((F,o) :)
_B o = ((B,o) :)
_P o = ((P,o) :)
_M o = ((M,o) :)

{-
V : T
S : +, -
ω : T
P : T -> F + T - T + B
-}

interp :: Distance -> (L,Order) -> Turtle Picture
interp d (F,o) = forward (d*0.8^^(9-o))
interp d (B,o) = backward (d*0.8^^(9-o))
interp _ (P,o) = left 15
interp _ (M,o) = right 30

_T :: Order -> LSystem
_T 0 = id 
_T (n+1) = _F n . _P n . _T n . _M n . _T n . _P n . _B n

tree :: Order -> Picture
tree n = translate (negate (2.0 ^ (n-1))) (negate (2.0 ^ (n-1)))
             $ mconcat
             $ evalState (sequence $ map (interp 100) (_T n []))
             $ TurtleState (0, 0) 0 True black

displayTree :: IO ()
displayTree = do
    { o <- read . head <$> getArgs
    ; let fac = 2 ^^ (8 - o)
    ; display window white $ translate (-150) (-50) $ rotate (negate 90) $ scale fac fac $ tree o
    }

window :: Display
window = InWindow "Sierpinski triangle" (800,800) (100,100)
