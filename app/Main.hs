module Main where

import Control.Concurrent.Chan
import System.Environment
import World
import Interact

main :: IO ()
main = do
    { och <- newChan 
    ; genericInteract
        (lines <$> getContents)
        (outputPictures och)
        run
    }
