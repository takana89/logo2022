{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module World where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

data World
    = World
    { turtle :: Turtle
    , pict   :: Picture 
    }
    deriving (Eq, Show)

defaultWorld :: World
defaultWorld = World { turtle = defaultTurtle 
                     , pict = blank
                     }
data Turtle
    = Turtle
    { position :: Point
    , direction :: Float
    , pen :: Updown
    , thic :: Float
    , color :: Color
    , showme :: Bool
    }
    deriving (Eq, Show)

defaultTurtle :: Turtle
defaultTurtle = Turtle
              { position = (0,0)
              , direction = 60
              , pen = Down
              , thic = 10
              , color = black
              , showme = False
              }

data Updown
    = Up
    | Down
    deriving (Eq, Show)

--

type Instruction = World -> World

forward, fd :: Float -> Instruction
forward d world = case world of
    World { turtle = turtle } 
        -> world { turtle = turtle { position = newpos turtle.direction d turtle.position }
                 , pict = case turtle.pen of
                    Up   -> blank
                    Down -> thicLine d turtle.direction turtle.thic turtle.position
                 }

fd = forward

back, bk :: Float -> Instruction
back = forward . negate

bk = back

newpos :: Float -> Float -> Point -> Point
newpos dir d pos0 = pos0 G.+ diff
    where
        diff = d G.* unitVectorAtAngle a
        a = degToRad (90 - dir)

spot :: Float -> Point -> Picture
spot thc (x,y)
    = translate x y
    $ circleSolid (thc / 2)

dispPict :: Picture -> IO ()
dispPict pict = display FullScreen white pict

thicLine :: Float -> Float -> Float -> Point -> Picture
thicLine dis dir thc pos1 
    = spot thc pos1
    <> spot thc pos2
    <> translate x y (rotate dir $ rectangleSolid thc dis)
    where
        (x,y) = 0.5 G.* (pos1 G.+ pos2)
        pos2 = newpos dir dis pos1

dispWorld :: World -> IO ()
dispWorld world = dispPict world.pict
