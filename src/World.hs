{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module World where

import Data.Bool
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import qualified Graphics.Gloss.Data.Picture as G
import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

data World
    = World
    { turtle :: Turtle
    , pict   :: Maybe Picture 
    , wrap   :: Wrap
    }
    deriving (Eq, Show)

data Wrap
    = Wrap
    | Window
    | Fence
    deriving (Eq, Show)

defaultWorld :: World
defaultWorld = World { turtle = defaultTurtle 
                     , pict = Just defaultTurtle.selfimg
                     , wrap = Wrap
                     }
data Turtle
    = Turtle
    { position :: Point
    , direction :: Float
    , pen :: Updown
    , thic :: Float
    , color :: Color
    , showme :: Bool
    , curimg :: Picture
    , selfimg :: Picture
    }
    deriving (Eq, Show)

defaultTurtle :: Turtle
defaultTurtle = Turtle
              { position = (0,0)
              , direction = initdir
              , pen = Down
              , thic = 10
              , color = black
              , showme = True
              , curimg = rotate initdir original
              , selfimg = original
              } 
              where
                original =  lineLoop [(-10,-10),(-10,10),(10,10),(10,-10),(-10,-10),(0,10),(10,-10)]
                initdir = 60

data Updown
    = Up
    | Down
    deriving (Eq, Show)

--

type Instruction = World -> World

forward, fd :: Float -> Instruction
forward d world = case world of
    World { turtle = turtle } 
        -> world { turtle = turtle { position = (x',y') 
                                   , curimg = translate x' y' (rotate turtle.direction turtle.selfimg)
                                   }
                 , pict = Just $ case turtle.pen of
                    Up   -> blank
                    Down -> thicLine d turtle.direction turtle.thic turtle.position
                 }
            where
                (x',y') = newpos turtle.direction d turtle.position

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
        -> world { turtle = turtle { direction = newdir 
                                   , curimg = uncurry translate turtle.position (rotate newdir turtle.selfimg)
                                   }
                 , pict = Just blank
                 }
            where
                newdir = turtle.direction + dir

rt = right

setxy :: Float -> Float -> Instruction
setxy x y world =  case world of
    World { turtle = turtle } 
        -> world { turtle = turtle { position = (x,y) 
                                   , curimg = translate x y turtle.selfimg
                                   }
                 , pict = Just $ case turtle.pen of
                    Up   -> blank
                    Down -> thicLine dist dir turtle.thic turtle.position
                 }
            where
                vec = (x,y) G.- turtle.position
                dist = magV vec
                dir = radToDeg (angleVV (0,1) vec)

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

setheading :: Float -> Instruction
setheading h world = case world of
    World { turtle = turtle } 
        -> right (h-d) world
        where
            d = turtle.direction

home :: Instruction
home world = case world of
    World { turtle = turtle } 
        -> setheading 0 (setxy 0 0 world )

arc :: Float -> Float -> Instruction
arc t r world = case world of
    World { turtle = turtle }
        -> world { turtle = turtle { direction = turtle.direction + t 
                                   , curimg = translate x y $ rotate (d+t) turtle.selfimg
                                   }
                 , pict = Just tarc 
                 }
            where
                (x,y) = turtle.position
                d = turtle.direction
                tarc = translate x y $ rotate d $ thickArc 0 t r turtle.thic

penup :: Instruction 
penup world = case world of
    World { turtle = turtle }
        -> world { turtle = turtle { pen = Up }
                 }

pendown :: Instruction 
pendown world = case world of
    World { turtle = turtle }
        -> world { turtle = turtle { pen = Down }
                 }

clean :: Instruction
clean world = world { pict = Nothing }

clearscreen :: Instruction
clearscreen = clean . home

showturtle :: Instruction
showturtle world = case world of
    World { turtle = turtle }
        -> world { turtle = turtle { showme = True }}

hideturtle :: Instruction
hideturtle world = case world of
    World { turtle = turtle }
        -> world { turtle = turtle { showme = False }}

wrapI :: Instruction
wrapI world = world { wrap = Wrap }

windowI :: Instruction
windowI world = world { wrap = Window }

fenceI :: Instruction
fenceI world = world { wrap = Fence }
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
dispWorld world = maybe (dispPict blank) dispPict world.pict

--

accumWorld :: [World] -> [Picture]
accumWorld = scanl accum blank

accum :: Picture -> World -> Picture
accum pict world = undefined