module Interact where

import Control.Concurrent.Chan
import Control.Concurrent
import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

genericInteract :: IO [a] -> ([b] -> IO ()) -> ([a] -> [b]) -> IO () 
genericInteract input output translate
    = output . translate =<< input

outputPictures :: Chan Picture -> [Picture] -> IO ()
outputPictures chan picts
    = forkIO (writeList2Chan chan picts) >> render chan

render :: Chan Picture -> IO ()
render chan
    = animateIO
        window0
        white
        (const (readChan chan))
        (const (return ()))

window0 :: Display
window0 = InWindow "logo2022 demo" (768,768) (768,0)