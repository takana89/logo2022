module Main where

import Control.Concurrent.Chan
import System.Environment
import System.IO.Unsafe
import System.Console.Haskeline
import World
import Interact

main :: IO ()
main = do
    { och <- newChan 
    ; genericInteract
        inputLines
        (outputPictures och)
        run
    }

inputLines :: IO [String]
inputLines 
    = unsafeInterleaveIO
    $ do
    { minput <- runInputT defaultSettings (getInputLine "")
    ; case minput of
        Nothing -> return []
        Just line -> (line :) <$> inputLines
    }
