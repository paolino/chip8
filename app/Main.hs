module Main (main) where

import Data.ByteString (readFile)
import Data.List (sort)
import Graphics (chip8Application)
import Rendering (run)
import State (State, bootState)
import System.Directory (listDirectory)
import Prelude hiding (readFile)

main :: IO ()
main = do
    files <- sort <$> listDirectory "roms"
    states <- traverse runFile $ ("roms/" <>) <$> files
    let games = zip files states
    run $ chip8Application games 0

runFile :: FilePath -> IO State
runFile = fmap bootState . readFile