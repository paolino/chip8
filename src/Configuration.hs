module Configuration
    ( configParserInfo
    , Config (..)
    , JColor (..)
    )
where

import Data.Word (Word8)
import Linear (V4 (..))
import Numeric (readHex)
import Options.Applicative
    ( Parser
    , ParserInfo
    , ReadM
    , auto
    , eitherReader
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , showDefault
    , showDefaultWith
    , strOption
    , value
    , (<**>)
    )

newtype JColor = JColor {colorJ :: V4 Word8}
    deriving (Show)

hexReader :: String -> Either String Word8
hexReader = nullLeft . readHex
  where
    nullLeft [] = Left "empty list"
    nullLeft ((x, _) : _) = Right x

jcolorReader :: ReadM JColor
jcolorReader = eitherReader $ \arg ->
    case arg of
        ['#', r1, r2, g1, g2, b1, b2] -> do
            r <- hexReader [r1, r2]
            g <- hexReader [g1, g2]
            b <- hexReader [b1, b2]
            pure $ JColor $ V4 r g b 255
        _ -> Left ("Cannot parse JColor: " ++ arg)

configParser :: Parser Config
configParser =
    Config
        <$> strOption
            ( long "roms-dir"
                <> metavar "ROMS_DIR"
                <> showDefault
                <> value "roms/test"
                <> help "Path to ROMs directory"
            )
        <*> strOption
            ( long "font-file"
                <> metavar "FONT_FILE"
                <> showDefault
                <> value "fonts/ShareTechMono-Regular.ttf"
                <> help "Path to font file"
            )
        <*> option
            auto
            ( long "pixel-size"
                <> metavar "PIXEL_SIZE"
                <> showDefault
                <> value 10
                <> help "Size of each pixel in pixels"
            )
        <*> option
            auto
            ( long "small-font"
                <> metavar "SMALL_FONT"
                <> showDefault
                <> value 24
                <> help "Size of small font in pixels"
            )
        <*> option
            auto
            ( long "large-font"
                <> metavar "LARGE_FONT"
                <> showDefault
                <> value 32
                <> help "Size of large font in pixels"
            )
        <*> option
            jcolorReader
            ( long "bg-color"
                <> metavar "BG_COLOR"
                <> showDefaultWith renderColor
                <> value (JColor $ V4 0 0 0 255)
                <> help "Background color in hex format"
            )
        <*> option
            jcolorReader
            ( long "game-color"
                <> metavar "GAME_COLOR"
                <> showDefaultWith renderColor
                <> value (JColor $ V4 255 255 255 255)
                <> help "Game color in hex format"
            )
        <*> option
            jcolorReader
            ( long "text-color"
                <> metavar "TEXT_COLOR"
                <> showDefaultWith renderColor
                <> value (JColor $ V4 255 128 0 255)
                <> help "Text color in hex format"
            )
        <*> option
            jcolorReader
            ( long "grid-color"
                <> metavar "GRID_COLOR"
                <> showDefaultWith renderColor
                <> value (JColor $ V4 128 128 128 255)
                <> help "Grid color in hex format"
            )
        <*> option
            auto
            ( long "speed"
                <> metavar "SPEED"
                <> showDefault
                <> value 10
                <> help "Instructions per frame"
            )
hex :: Word8 -> Char
hex x
    | x < 10 = toEnum $ fromEnum '0' + fromEnum x
    | otherwise = toEnum $ fromEnum 'A' + fromEnum x - 10

renderColor :: JColor -> String
renderColor (JColor (V4 r g b _)) =
    let renderHex x =
            let (h, l) = (x `div` 16, x `mod` 16)
            in  [hex h, hex l]
    in  "#" <> renderHex r <> renderHex g <> renderHex b

configParserInfo :: ParserInfo Config
configParserInfo =
    info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc "CHIP-8 Emulator v0.1.0.0, (C) 2023, Paolo Veronelli, Gabriele Lana"
        )
data Config = Config
    { romsDir :: FilePath
    , fontFile :: FilePath
    , pixelSize :: Int
    , smallFont :: Int
    , largeFont :: Int
    , bgColor :: JColor
    , gameColor :: JColor
    , textColor :: JColor
    , gridColor :: JColor
    , speed :: Int
    }
    deriving (Show)
