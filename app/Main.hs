{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.ByteString (readFile)
import Data.List (sort)
import Graphics (chip8Application)
import Rendering (GraphicsParams (..), run)
import State (State, bootState)
import System.Console.CmdArgs
import System.Directory (listDirectory)
import Prelude hiding (readFile)

data Config = Config
    { romsDir :: FilePath
    , fontFile :: FilePath
    , pixelSize :: Int
    , smallFont :: Int
    , largeFont :: Int
    }
    deriving (Show, Data, Typeable)

config :: Config
config =
    Config
        { romsDir = "roms" &= help "Path to ROMs directory"
        , fontFile = "fonts/VT323-Regular.ttf" &= typFile &= help "Path to font file"
        , pixelSize = 10 &= help "Size of each pixel in pixels"
        , smallFont = 24 &= help "Size of small font in pixels"
        , largeFont = 32 &= help "Size of large font in pixels"
        }
        &= summary "CHIP-8 Emulator v0.1.0.0, (C) 2023, Paolo Veronelli, Gabriele Lana"

main :: IO ()
main = do
    Config{..} <- cmdArgs config
    files <- sort <$> listDirectory romsDir
    states <- traverse runFile $ ((romsDir <> "/") <>) <$> files
    let games = zip files states
        gp = GraphicsParams
            do fromIntegral pixelSize
            do fromIntegral smallFont
            do fromIntegral largeFont
            do fontFile
    run gp $ chip8Application games 0

runFile :: FilePath -> IO State
runFile = fmap bootState . readFile