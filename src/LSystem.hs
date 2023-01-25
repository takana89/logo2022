{-# LANGUAGE NPlusKPatterns #-}
module LSystem where

import Control.Monad.State
import Data.List
import Graphics.Gloss
import Turtle

class LSystem a where
  axiom :: Int -> [a]
  rules :: [Int -> ([a] -> [a])]
  interp :: a -> Turtle Picture
  
data Hilbert = F | M | P deriving Eq

type HilbertSys = [Hilbert] -> [Hilbert]

_F, _M, _P :: HilbertSys
_F = (F :)
_M = (M :)
_P = (P :)

_X, _Y :: Int -> HilbertSys
_X 0     = id
_X (n+1) = _P . _Y n . _F . _M . _X n . _F . _X n . _M . _F . _Y n . _P
_Y 0     = id
_Y (n+1) = _M . _X n . _F . _P . _Y n . _F . _Y n . _P . _F . _X n . _M

instance LSystem Hilbert where
  axiom n = _X n []
  rules = [_X, _Y]
  interp F = forward 1
  interp M = right 90
  interp P = left 90
{-
drawLSystem :: LSystem a => a -> Float -> Int -> IO ()
drawLSystem _ s n = scanl1 (<>) 
                  $ filter (/= Blank)
                  $ evalState (mapM interp (axiom n))
                  $ TurtleState (0.5, 0.5) 0 True black
-}
