{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Configuration (Config (..), JColor (..), configParserInfo)
import Data.ByteString (readFile)
import Data.List (sort)
import Graphics (chip8Application)
import Options.Applicative (execParser)
import Rendering (GraphicsParams (..), run)
import State (State, bootState)
import System.Directory (listDirectory)
import Prelude hiding (readFile)

main :: IO ()
main = do
    Config{..} <- execParser configParserInfo
    files <- sort <$> listDirectory romsDir
    states <- traverse runFile $ ((romsDir <> "/") <>) <$> files
    let games = zip files states
        gp = GraphicsParams
            do fromIntegral pixelSize
            do fromIntegral smallFont
            do fromIntegral largeFont
            do fontFile
            do colorJ bgColor
            do colorJ gameColor
            do colorJ textColor
            do colorJ gridColor
    run gp $ chip8Application speed games 0

runFile :: FilePath -> IO State
runFile = fmap bootState . readFile
