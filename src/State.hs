{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module State
    ( State (..)
    , bootState
    , readSprite
    , pasteSprite
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
import Data.Word (Word16)
import Numeric (showHex)
import Offset (addHeaderSpace, memoryOffset)
import Opcodes (decode)
import Types
    ( Address
    , Byte (..)
    , Coo (..)
    , Display
    , Height
    , Memory
    , Opcode
    , Registers
    , Sprite
    )

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
    , indexRegister :: Address
    , programCounter :: Address
    , stack :: [Address]
    , delayTimer :: Word16
    , soundTimer :: Word16
    , memory :: Memory
    , display :: Display
    }
    deriving (Show, Eq)

loadProgram :: ByteString -> Memory
loadProgram =
    Map.fromList
        . zip [0 ..]
        . addHeaderSpace
        . fmap Byte
        . B.unpack

bootState :: ByteString -> State
bootState program =
    State
        { registers = Map.empty
        , indexRegister = 0
        , programCounter = memoryOffset
        , stack = []
        , delayTimer = 0
        , soundTimer = 0
        , memory = loadProgram program
        , display = Map.empty
        }

readSprite :: Height -> State -> Sprite
readSprite h State{..} = do
    i <- [0 .. h - 1]
    let row = memory Map.! (indexRegister + fromIntegral i)
    pure $ do
        j <- [0 .. 7]
        pure $ row `shiftR` (7 - j) .&. 1 == 1

pasteSprite :: Coo -> Sprite -> Display -> (Bool, Display)
pasteSprite (Coo x y) sprite display = foldl' xor' (False, display) $ do
    (i, row) <- zip [0 ..] sprite
    (j, pixel) <- zip [0 ..] row
    pure (Coo ((x + j) `mod` 64) ((y + i) `mod` 32), pixel)
  where
    xor' :: (Bool, Display) -> (Coo, Bool) -> (Bool, Display)
    xor' (v, d) (coo, p) =
        let p' = Map.findWithDefault False coo d
            p'' = p' `xor` p
        in  -- v' = p' /= p''

            (v || (p && p'), Map.insert coo p'' d)

retrieveInstruction :: Address -> Memory -> Opcode
retrieveInstruction n m =
    fromIntegral (m Map.! n) `shiftL` 8 + fromIntegral (m Map.! (n + 1))

render :: State -> String
render State{..} = concat $ do
    y <- [0 .. 31]
    pure $ (<> "\n") $ do
        x <- [0 .. 63]
        pure $ if Map.findWithDefault False (Coo x y) display then '█' else ' '
