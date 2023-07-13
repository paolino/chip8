module Main (main) where

import Control.Monad ((>=>))
import Data.ByteString (readFile)
import Graphics (chip8Application)
import Rendering (run)
import State (bootState)
import Prelude hiding (readFile)

main :: IO ()
main = do
    runFile "roms/1-chip8-logo.ch8"
    runFile "roms/2-ibm-logo.ch8"
    runFile "roms/3-corax+.ch8"
    runFile "roms/4-flags.ch8"
    runFile "roms/5-quirks.ch8"
    runFile "roms/6-keypad.ch8"

runFile :: FilePath -> IO ()
runFile = readFile >=> run . chip8Application . bootState