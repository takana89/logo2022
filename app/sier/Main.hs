module Main where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Interact
import Sierpinski

main :: IO ()
main = displaySier
{- --
main = do
    { och <- newChan
    ; var <- newMVar undefined
    ; outputPictures var och (sierpinski' 6)
    }
-- -}
