{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module World where

import Data.Bool
import Data.Maybe
import Graphics.Gloss hiding (arc)
import Graphics.Gloss.Data.Color
import qualified Graphics.Gloss.Data.Picture as G
import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

data World
    = World
    { control :: [String]
    , instructions :: [Instruction]
    , acc :: Float
    , accb :: Bool
    , environment :: Env
    , turtle :: Turtle
    , pict   :: Maybe Picture 
    , wrap   :: Wrap
    }

type Env = [(String,Float)]

data Wrap
    = Wrap
    | Window
    | Fence
    deriving (Eq, Show)

defaultWorld :: World
defaultWorld = World { control = []
                     , instructions = defaultInstructions
                     , acc = 0
                     , accb = True
                     , environment = []
                     , turtle = defaultTurtle 
                     , pict = Just (Pictures [defaultTurtle.curimg, blank])
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
                initdir = 90

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
accumWorld = scanl accum (Pictures [blank,blank])

accum :: Picture -> World -> Picture
accum pict world = case world of
    World { turtle = turtle
          , pict = mpict
          , wrap = wrap
          } -> case mpict of
            Nothing -> blank
            Just pic -> case pict of
                Pictures [_, pics] -> Pictures [turtle.curimg, pics <> pic]
                _ -> error $ show pict

--

eval :: World -> [World]
eval world = world : rests
    where 
        rests
            | final world = []
            | otherwise = eval (step world)

final :: World -> Bool
final world = null world.instructions

step :: World -> World
step world = case world.control of 
    _:cs -> world' { control = cs 
                   , instructions = tail world.instructions
                   }
    where
        world' = head world.instructions world

run :: [String] -> [Picture]
run cs 
    = accumWorld 
    $ eval
    $ defaultWorld { control = cs }

defaultInstructions :: [Instruction]
defaultInstructions 
    = [ fd 200
      , lt 90
      , fd 190
      , lt 90
      , fd 180
      , lt 90
      , fd 170
      , lt 90
      , fd 160
      ]

proc0s :: [(String, Instruction)]
proc0s = [ ("home", home)
         , ("penup", penup)
         , ("pendown", pendown)
         , ("clean", clean)
         , ("clearscreen", clearscreen)
         ]
proc1s :: [(String, Float -> Instruction)]
proc1s = [ ("fd", fd)
         , ("forward", forward)
         , ("bk", bk)
         , ("back", back)
         , ("lt", lt)
         , ("left", left)
         , ("rt", rt)
         , ("right", right)
         , ("setx", setx)
         , ("sety", sety)
         , ("setheading", setheading)
         ]
proc2s :: [(String, Float -> Float -> Instruction)]
proc2s = [ ("setxy", setxy)
         , ("arc", arc)
         ]
arity :: [(String,Int)]
arity =  [ ("home", 0)
         , ("penup", 0)
         , ("pendown", 0)
         , ("clean", 0)
         , ("clearscreen", 0)
         ] ++
         [ ("fd", 1)
         , ("foreward", 1)
         , ("bk", 1)
         , ("back", 1)
         , ("lt", 1)
         , ("left", 1)
         , ("rt", 1)
         , ("right", 1)
         , ("setx", 1)
         , ("sety", 1)
         , ("setheading", 1)
         ] ++
         [ ("setxy", 2)
         , ("arc", 2)
         ]