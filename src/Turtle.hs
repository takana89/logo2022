module Turtle
    ( Turtle
    , TurtleState (..)
    , Position
    , Direction
    , Distance
    , forward
    , backward
    , left
    , right
    , goto
    , setx
    , sety
    , seth
    , setc
    , pendown
    , penup
    , home
    , torad
    , todeg
    ) where

import Control.Monad.State
import Graphics.Gloss

type Turtle = State TurtleState

data TurtleState = TurtleState
    { _cur :: Position
    , _dir :: Direction
    , _pen :: Bool
    , _col :: Color
    }

type Position = (Float, Float)
type Direction = Float
type Distance = Float

torad :: Float -> Float
torad d = d * pi / 180

todeg :: Float -> Float
todeg r = r * 180 / pi

add :: Distance -> Direction -> Position -> Position
add dis dir (x,y) = (x + dis * cos (torad dir), y + dis * sin (torad dir))

forward :: Distance -> Turtle Picture
forward dis = do
    { s <- get
    ; let { pos = _cur s
          ; dir = _dir s
          ; newpos = add dis dir pos
          }
    ; put (s {_cur = newpos})
    ; return $ if _pen s then color (_col s) (line [pos, newpos]) else Blank
    }

backward :: Distance -> Turtle Picture
backward = forward . negate

left :: Direction -> Turtle Picture
left dir = do
    { s <- get
    ; put (s {_dir = _dir s + dir})
    ; return Blank
    }

right :: Direction -> Turtle Picture
right = left . negate

goto :: Position -> Turtle Picture
goto pos = do
  { s <- get
  ; let old = _cur s
  ; put (s {_cur = pos})
  ; return $ if _pen s then (color (_col s) (line [old, pos])) else Blank
  }

setx :: Float -> Turtle Picture
setx x = do
  { s <- get
  ; let (_,y) = _cur s
  ; put (s {_cur = (x,y)})
  ; return Blank
  }
  
sety :: Float -> Turtle Picture
sety y = do
  { s <- get
  ; let (x,_) = _cur s
  ; put (s {_cur = (x,y)})
  ; return Blank
  }

seth :: Float -> Turtle Picture
seth ang = do
  { s <- get
  ; put (s {_dir = ang})
  ; return Blank
  }

setc :: Color -> Turtle Picture
setc col = do
  { s <- get
  ; put (s {_col = col})
  ; return Blank
  }

pendown :: Turtle Picture
pendown = do
  { s <- get
  ; put (s {_pen = True})
  ; return Blank
  }

penup :: Turtle Picture
penup= do
  { s <- get
  ; put (s {_pen = False})
  ; return Blank
  }

home :: Turtle Picture
home = do
  { s <- get
  ; put (s {_cur = (0,0), _dir = 0})
  ; return Blank
  }
