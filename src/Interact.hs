{-# LANGUAGE ScopedTypeVariables #-}
module Interact where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

genericInteract :: IO [a] -> ([b] -> IO ()) -> ([a] -> [b]) -> IO () 
genericInteract input output translate
    = output . translate =<< input

outputPictures :: MVar Picture -> Chan Picture -> [Picture] -> IO ()
outputPictures var chan picts
    = forkIO (writeList2Chan chan picts) >> render' var chan

render :: MVar Picture -> Chan Picture -> IO ()
render var chan
    = animateIO
        window0
        white
        (const (readingChan var chan))
        (const (return ()))

render' :: MVar Picture -> Chan Picture -> IO ()
render' var chan
    = animateIO
        window0
        white
        (const (readChan chan))
        (const (return ()))


readingChan :: MVar Picture -> Chan Picture -> IO Picture
readingChan var chan 
    = catch (do { next <- readChan chan
                ; modifyMVar_ var (const (return next))
                ; return next
                })
            (\ BlockedIndefinitelyOnMVar
                -> do { prev <- readMVar var
                      ; return prev
                      })

-- render :: Chan Picture -> IO ()
-- render chan
--     = animateIO
--         window0
--         white
--         (const (readChan chan))
--         (const (return ()))

window0 :: Display
window0 = InWindow "logo2022 demo" (768,768) (768,0)