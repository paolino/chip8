module Main (main) where

import Control.Monad ((>=>))
import Data.ByteString (readFile)
import Graphics (game)
import State (bootState)
import Terminal.Game (playGame)
import Prelude hiding (readFile)

main :: IO ()
main = runFile "roms/IBM Logo.ch8"

runFile :: FilePath -> IO ()
runFile = readFile >=> playGame . game . bootState