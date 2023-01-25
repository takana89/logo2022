{-# LANGUAGE NPlusKPatterns #-}
module Hilbert where

import System.Environment
import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Turtle

type Order = Int

data L = F | M | P deriving Eq

type LSystem = [L] -> [L]

instance Show L where
  show F = "F"
  show M = "-"
  show P = "+"

f, m, p :: LSystem
f = (F:)
m = (M:)
p = (P:)

x, y :: Order -> LSystem

x 0     = id
x (n+1) = p . y n . f . m . x n . f . x n . m . f . y n . p

y 0     = id
y (n+1) = m . x n . f . p . y n . f . y n . p . f . x n . m

interp :: Distance -> L -> Turtle Picture
interp d F = forward d
interp _ P = left 90
interp _ M = right 90

hilbert :: Order -> Picture
hilbert n = translate (negate (2.0^(n-1))) (negate (2.0^(n-1)))
          $ mconcat
          $ evalState (sequence $ map (interp 2) (x n []))
          $ TurtleState (0.5, 0.5) 0 True black
          
hilbert' :: Order -> [Picture]
hilbert' n = scanl1 (<>)
           $ map (scale 8 8)
           $ map (translate (negate (2.0^(n-1))) (negate (2.0^(n-1))))
           $ filter (/= Blank)
           $ evalState (sequence $ map (interp 1) (x n []))
           $ TurtleState (0.5, 0.5) 0 True black

drawing :: [Picture] -> Picture
drawing = head

next :: ViewPort -> Float -> [Picture] -> [Picture]
next _ _ = tail

ini :: [Picture]
ini = cycle $ hilbert' 6

curve :: Float -> Picture
curve x = let
    { t = floor x `mod` 9 + 1
    ; fact = fromIntegral (2^ (10 - t))
    } in scale fact fact (hilbert t)

window :: Display
window = InWindow "Hilbert curve" (2000, 2000) (100, 100)

