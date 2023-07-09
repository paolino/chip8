module Main (main) where

import Control.Monad ((>=>))
import Data.ByteString (readFile)
import Encoding (testInterpreter, testProgram)
import Graphics (game)
import Programs.Dump (dumpPrograms)
import State (bootState)
import Terminal.Game (playGame)
import Prelude hiding (readFile)

main :: IO ()
main = do
    dumpPrograms
    runFile "roms/IBM Logo'.ch8"

runFile :: FilePath -> IO ()
runFile = readFile >=> playGame . game . bootState