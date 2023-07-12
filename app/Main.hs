module Main (main) where

import Control.Monad ((>=>))
import Data.ByteString (readFile)
import Graphics (game)
import Programs.Dump (dumpPrograms)
import State (bootState)
import Terminal.Game (playGame)
import Prelude hiding (readFile)

main :: IO ()
main = do
    -- compile "roms/IBM Logo'.ch8"
    dumpPrograms
    -- TODO: add program selection in roms directory
    -- runFile "roms/IBM Logo'.ch8"
    runFile "roms/Subroutine.ch8"

runFile :: FilePath -> IO ()
runFile = readFile >=> playGame . game . bootState