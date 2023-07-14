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
    { roms :: FilePath
    , pixelSize :: Int
    , smallFont :: Int
    , largeFont :: Int
    }
    deriving (Show, Data, Typeable)

config :: Config
config =
    Config
        { roms = def &= argPos 0 &= typ "ROM"
        , pixelSize = 10 &= help "Size of each pixel in pixels"
        , smallFont = 24 &= help "Size of small font in pixels"
        , largeFont = 32 &= help "Size of large font in pixels"
        }
        &= summary "CHIP-8 Emulator v0.1.0.0, (C) 2023, Paolo Veronelli, Gabriele Lana"
        
main :: IO ()
main = do
    Config{..} <- cmdArgs config
    files <- sort <$> listDirectory roms
    states <- traverse runFile $ ((roms <> "/") <>) <$> files
    let games = zip files states
        gp = GraphicsParams
            do fromIntegral pixelSize
            do fromIntegral smallFont
            do fromIntegral largeFont
    run gp $ chip8Application games 0

runFile :: FilePath -> IO State
runFile = fmap bootState . readFile