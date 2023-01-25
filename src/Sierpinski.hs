{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sierpinski where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Environment
import Turtle

type Order = Int

data L = F | M | P deriving Eq

instance Show L where
    show F = "F"
    show M = "-"
    show P = "+"

type LSystem = [L] -> [L]

f, p, m :: LSystem
f = (F :)
p = (P :)
m = (M :)

{-
V ： A, B
S ： +, -
ω： A
P ： (A → B−A−B), (B → A+B+A)
-}

_A, _B :: Order -> LSystem
_A 0 = f
_A (n+1) = _B n . m . _A n . m . _B n
_B 0 = f
_B (n+1) = _A n . p . _B n . p . _A n

fd, bk :: Distance -> Turtle Picture
fd = forward
bk = backward
lt, rt :: Direction -> Turtle Picture
lt = left
rt = right

interp :: Distance -> L -> Turtle Picture
interp d F = fd d
interp _ P = lt 60
interp _ M = rt 60

sierpinski :: Order -> Picture
sierpinski n = translate (negate (2.0 ^ (n-1))) (negate (2.0 ^ (n-1)))
             $ mconcat
             $ evalState (sequence $ map (interp 1) (_A n []))
             $ TurtleState (0,0) 0 True black

sierpinski' :: Order -> [Picture]
sierpinski' n = map (translate (negate 320) (negate 280)) 
              $ scanl1 (<>)
              $ evalState (sequence $ map (interp 10) (_A n []))
              $ TurtleState (0,0) 0 True black


displaySier :: IO ()
displaySier = do
    { (os :: [Int]) <- map read <$> getArgs
    ; let fac = 2 -- ^^ (9 - o)
    ; display window white $ scale fac fac $ sierpinski 8
    }

window :: Display
window = InWindow "Sierpinski triangle" (800,800) (100,100)
