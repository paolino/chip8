module Main (main) where

import Control.Monad ((>=>))
import Data.ByteString (readFile)
import Graphics (chip8Application)
-- import Programs.Dump (dumpPrograms)
import Rendering (run, testApplication)
import State (bootState)
import Prelude hiding (readFile)

main :: IO ()
main = do
    run testApplication
    -- compile "roms/IBM Logo'.ch8"
    -- dumpPrograms
    -- TODO: add program selection in roms directory
    runFile "roms/IBM Logo.ch8"
    -- runFile "roms/Subroutine.ch8"
    -- runFile "roms/3-corax+.ch8"
    --  runFile "roms/4-flags.ch8"
    -- runFile "roms/5-quirks.ch8"

runFile :: FilePath -> IO ()
runFile = readFile >=> run . chip8Application . bootState