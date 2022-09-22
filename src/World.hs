{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module World where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
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
              , pen = Up
              , thic = 1
              , color = black
              , showme = False
              }

data Updown
    = Up
    | Down
    deriving (Eq, Show)

--

type Instruction = World -> World

forward :: Float -> Instruction
forward d world = case world of
    World { turtle = turtle } -> world { turtle = turtle { position = newpos turtle.direction d turtle.position }}

newpos :: Float -> Float -> Point -> Point
newpos dir d pos0 = pos0 G.+ diff
    where
        diff = d G.* unitVectorAtAngle a
        a = degToRad (90 - dir)
