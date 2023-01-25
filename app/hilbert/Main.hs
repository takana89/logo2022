module Main where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Hilbert
import Interact

main :: IO ()
main = do
    { och <- newChan
    ; var <- newMVar blank
    ; outputPictures var och (hilbert' 6)
    }