{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module State
    ( State (..)
    , bootState
    , readSprite
    , draw
    , retrieveInstruction
    , render
    , renderState
    ) where

import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Opcodes (decode)
import Types (Display, Memory, Registers, Sprite, Word12, Word4)

renderState :: State -> String
renderState State{..} =
    intercalate
        "\n"
        [ "PC: " <> show programCounter
        , "I: " <> show indexRegister
        , "DT: " <> show delayTimer
        , "ST: " <> show soundTimer
        , "SP: " <> show stack
        , "Registers: " <> show registers
        , "Opcode: " <> showHex (retrieveInstruction programCounter memory) ""
        , "Instruction: " <> show (decode $ retrieveInstruction programCounter memory)
        ]

data State = State
    { registers :: Registers
    , indexRegister :: Word12
    , programCounter :: Word12
    , stack :: [Word16]
    , delayTimer :: Word16
    , soundTimer :: Word16
    , memory :: Memory
    , display :: Display
    }
    deriving (Show, Eq)

loadProgram :: ByteString -> Memory
loadProgram = Map.fromList . zip [0 ..] . (replicate 512 0 <>) . B.unpack

bootState :: ByteString -> State
bootState program =
    State
        { registers = Map.empty
        , indexRegister = 0
        , programCounter = 0x200
        , stack = []
        , delayTimer = 0
        , soundTimer = 0
        , memory = loadProgram program
        , display = Map.empty
        }

readSprite :: Word4 -> State -> Sprite
readSprite h State{..} = do
    i <- [0 .. h - 1]
    let row = memory Map.! (indexRegister + fromIntegral i)
    pure $ do
        j <- [0 .. 7]
        pure $ row `shiftR` (7 - j) .&. 1 == 1

draw :: Word8 -> Word8 -> Sprite -> Display -> Display
draw x y sprite display = foldl' xor' display $ do
    (i, row) <- zip [0 ..] sprite
    (j, pixel) <- zip [0 ..] row
    pure ((x + j) `mod` 64, (y + i) `mod` 32, pixel)
  where
    xor' :: Display -> (Word8, Word8, Bool) -> Display
    xor' d (x', y', p) = Map.insertWith xor (x', y') p d

retrieveInstruction :: Word12 -> Memory -> Word16
retrieveInstruction n m =
    fromIntegral (m Map.! n) `shiftL` 8 .|. fromIntegral (m Map.! (n + 1))

render :: State -> String
render State{..} = concat $ do
    y <- [0 .. 31]
    pure $ (<> "\n") $ do
        x <- [0 .. 63]
        pure $ if Map.findWithDefault False (x, y) display then '█' else ' '