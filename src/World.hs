{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module World where

import Data.Bool
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
                     , pict = defaultTurtle.selfimg
                     }
data Turtle
    = Turtle
    { position :: Point
    , direction :: Float
    , pen :: Updown
    , thic :: Float
    , color :: Color
    , showme :: Bool
    , selfimg :: Picture
    }
    deriving (Eq, Show)

defaultTurtle :: Turtle
defaultTurtle = Turtle
              { position = (0,0)
              , direction = 60
              , pen = Down
              , thic = 10
              , color = black
              , showme = True
              , selfimg = lineLoop [(-10,-10),(-10,10),(10,10),(10,-10),(-10,-10),(0,10),(10,-10)]
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
        -> world { turtle = turtle { position = (x',y') }
                 , pict = bool blank selfimg' turtle.showme <> case turtle.pen of
                    Up   -> blank
                    Down -> thicLine d turtle.direction turtle.thic turtle.position
                 }
            where
                (x',y') = newpos turtle.direction d turtle.position
                selfimg' = translate x' y' (rotate turtle.direction turtle.selfimg)

fd = forward

back, bk :: Float -> Instruction
back = forward . negate

bk = back

left, lt :: Float -> Instruction
left = right . negate

lt = left

right, rt :: Float -> Instruction 
right dir world = case world of
    World { turtle = turtle } 
        -> world { turtle = turtle { direction = newdir }
                 , pict = bool blank selfimg' turtle.showme
                 }
            where
                newdir = turtle.direction + dir
                selfimg' = uncurry translate turtle.position (rotate newdir turtle.selfimg)

rt = right

setxy :: Float -> Float -> Instruction
setxy x y world =  case world of
    World { turtle = turtle } 
        -> world { turtle = turtle { position = (x,y) }
                 , pict = bool blank selfimg' turtle.showme <> case turtle.pen of
                    Up   -> blank
                    Down -> thicLine dist dir turtle.thic turtle.position
                 }
            where
                vec = (x,y) G.- turtle.position
                dist = magV vec
                dir = radToDeg (angleVV (0,1) vec)
                selfimg' = translate x y turtle.selfimg

setx :: Float -> Instruction
setx x world = case world of
    World { turtle = turtle } 
        -> setxy x y world
        where
            (_,y) = turtle.position

sety :: Float -> Instruction
sety y world = case world of
    World { turtle = turtle } 
        -> setxy x y world
        where
            (x,_) = turtle.position

-- utilities

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
