module Sprites
    ( hexadecimalSprite
    , storeHexadecimalSprites
    , canLoadHexadecimalSprites
    ) where

import Data.List (foldl')
import Data.Map qualified as Map
import Offset (memoryOffset)
import Types (Address, Byte, Memory, Nibble)

hexadecimalSprite :: Nibble -> Address
hexadecimalSprite = fromIntegral . (* 5)

storeHexadecimalSprites :: Memory -> Memory
storeHexadecimalSprites m =
    foldl' (\acc (i, x) -> Map.insert i x acc) m . zip [0 ..]
        $ hexadecimalSpritesMemory

canLoadHexadecimalSprites :: Bool
canLoadHexadecimalSprites =
    memoryOffset >= fromIntegral (length hexadecimalSpritesMemory)

bin :: Integer -> Byte
bin = go id
  where
    go :: (Byte -> Byte) -> Integer -> Byte
    go f 0 = f 0
    go f n = go (\acc -> f $ acc * 2 + fromIntegral (n `mod` 10)) (n `div` 10)
hexadecimalSpritesMemory :: [Byte]
hexadecimalSpritesMemory =
    fmap
        (* 16)
        [ -- 0
          bin 1111
        , bin 1001
        , bin 1001
        , bin 1001
        , bin 1111
        , -- 1
          bin 0010
        , bin 0110
        , bin 0010
        , bin 0010
        , bin 0111
        , -- 2
          bin 1111
        , bin 0001
        , bin 1111
        , bin 1000
        , bin 1111
        , -- 3
          bin 1111
        , bin 0001
        , bin 1111
        , bin 0001
        , bin 1111
        , -- 4
          bin 1001
        , bin 1001
        , bin 1111
        , bin 0001
        , bin 0001
        , -- 5
          bin 1111
        , bin 1000
        , bin 1111
        , bin 0001
        , bin 1111
        , -- 6
          bin 1111
        , bin 1000
        , bin 1111
        , bin 1001
        , bin 1111
        , -- 7
          bin 1111
        , bin 0001
        , bin 0010
        , bin 0100
        , bin 0100
        , -- 8
          bin 1111
        , bin 1001
        , bin 1111
        , bin 1001
        , bin 1111
        , -- 9
          bin 1111
        , bin 1001
        , bin 1111
        , bin 0001
        , bin 1111
        , -- A
          bin 1111
        , bin 1001
        , bin 1111
        , bin 1001
        , bin 1001
        , -- B
          bin 1110
        , bin 1001
        , bin 1110
        , bin 1001
        , bin 1110
        , -- C
          bin 1111
        , bin 1000
        , bin 1000
        , bin 1000
        , bin 1111
        , -- D
          bin 1110
        , bin 1001
        , bin 1001
        , bin 1001
        , bin 1110
        , -- E
          bin 1111
        , bin 1000
        , bin 1111
        , bin 1000
        , bin 1111
        , -- F
          bin 1111
        , bin 1000
        , bin 1111
        , bin 1000
        , bin 1000
        ]
