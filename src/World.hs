{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module World where

import Control.Monad.State
import Data.Bool
import Data.Maybe
import Graphics.Gloss hiding (arc)
import Graphics.Gloss.Data.Color
import qualified Graphics.Gloss.Data.Picture as G
import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Syntax

data World
    = World
    { control :: [String]
    , instructions :: [Instruction]
    , acc :: Value
    , turtle :: Turtle
    , pict   :: Maybe Picture 
    , wrap   :: Wrap
    , valenv :: ValEnv
    , procenv :: ProcEnv
    , pictflag :: Bool
    }


data Wrap
    = Wrap
    | Window
    | Fence
    deriving (Eq, Show)

defaultWorld :: World
defaultWorld = World { control = []
                     , instructions = defaultInstructions
                     , acc = Float 0
                     , turtle = defaultTurtle 
                     , pict = Just (Pictures [defaultTurtle.curimg, blank])
                     , wrap = Wrap
                     , valenv = []
                     , procenv = []
                     , pictflag = True
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
              , thic = 1
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

type Instruction = World -> (World, [World])

wrap :: a -> [a]
wrap x = [x]

forward, fd :: Float -> Instruction
forward d world = case world of
    World { turtle = turtle } 
        -> (,) <*> wrap
        $  world { turtle = turtle { position = (x',y') 
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
        -> (,) <*> wrap 
        $  world { turtle = turtle { direction = newdir 
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
        -> (,) <*> wrap 
        $  world { turtle = turtle { position = (x,y) 
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
home world = case setxy 0 0 world of
    (w,_) -> setheading 0 w

arc :: Float -> Float -> Instruction
arc t r world = case world of
    World { turtle = turtle }
        -> (,) <*> wrap
        $  world { turtle = turtle { direction = turtle.direction + t 
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
        -> (,) <*> wrap
        $  world { turtle = turtle { pen = Up }
                 }

pendown :: Instruction 
pendown world = case world of
    World { turtle = turtle }
        -> (,) <*> wrap
        $  world { turtle = turtle { pen = Down }
                 }

clean :: Instruction
clean world = (,) <*> wrap
            $ world { pict = Nothing }

clearscreen :: Instruction
clearscreen world = case home world of
    (w,_) -> case clean w of
        (w',_) -> (w',[w,w'])

showturtle :: Instruction
showturtle world = case world of
    World { turtle = turtle }
        -> (,) <*> wrap
        $  world { turtle = turtle { showme = True }}

hideturtle :: Instruction
hideturtle world = case world of
    World { turtle = turtle }
        -> (,) <*> wrap
        $  world { turtle = turtle { showme = False }}

wrapI :: Instruction
wrapI world = (,) <*> wrap $ world { wrap = Wrap }

windowI :: Instruction
windowI world = (,) <*> wrap $ world { wrap = Window }

fenceI :: Instruction
fenceI world = (,) <*> wrap $ world { wrap = Fence }

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

exec :: World -> [World]
exec world = world : rests
    where 
        rests
            | final world = []
            | otherwise = exec (step world)

final :: World -> Bool
final world = null world.instructions

step :: World -> World
step world = case world.control of 
    _:cs -> world' { control = cs 
                   , instructions = tail world.instructions
                   }
    where
        (world',_) = head world.instructions world

run :: [String] -> [Picture]
run cs 
    = accumWorld 
    $ exec
    $ defaultWorld { control = repeat "" ++ cs }

defaultInstructions :: [Instruction]
-- defaultInstructions 
--     = [ fd 200
--       , lt 90
--       , fd 190
--       , lt 90
--       , fd 180
--       , lt 90
--       , fd 170
--       , lt 90
--       , fd 160
--       ]
defaultInstructions
    = concat
    $ replicate 90
    $ concat
    $ replicate 4
    [ fd 200 
    , lt 89
    ]

type Builtin = (Arity, Procedure0)
type Arity = Int
type Procedure0 = [Float] -> Instruction

proc0s :: [(String, Builtin)]
proc0s = [ ("home", (0, const home))
         , ("penup", (0, const penup))
         , ("pendown", (0, const pendown))
         , ("clean", (0, const clean))
         , ("clearscreen", (0, const clearscreen))
         ]
proc1s :: [(String, Builtin)]
proc1s = [ ("fd", (1, mkproc1 fd ))
         , ("forward", (1, mkproc1 forward))
         , ("bk", (1, mkproc1 bk))
         , ("back", (1, mkproc1 back))
         , ("lt", (1, mkproc1 lt))
         , ("left", (1, mkproc1 left))
         , ("rt", (1, mkproc1 rt))
         , ("right", (1, mkproc1 right))
         , ("setx", (1, mkproc1 setx))
         , ("sety", (1, mkproc1 sety))
         , ("setheading", (1, mkproc1 setheading))
         ]
    where
        mkproc1 f [x] = f x
proc2s :: [(String, Builtin)]
proc2s = [ ("setxy", (2, mkproc2 setxy))
         , ("arc", (2, mkproc2 arc))
         ]
    where
        mkproc2 g [x,y] = g x y

builtins :: [(String, Builtin)]
builtins = proc0s ++ proc1s ++ proc2s
