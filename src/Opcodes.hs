{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Opcodes (Instruction (..), decode) where

import Data.Bits (Bits (..))
import Data.Word (Word16, Word8)
import Types (Word12, Word4)

-- putStrLn $ showHex 12 "" -- prints "c"
-- putStrLn $ showIntAtBase 2 intToDigit 12 "" -- prints "1100"
-- 00E0 (clear screen)
-- 1NNN (jump)
-- 6XNN (set register VX)
-- 7XNN (add value to register VX)
-- ANNN (set index register I)
-- DXYN (display/draw)

data Instruction
    = ClearScreen
    | Jump Word12
    | SetRegister Word4 Word8
    | AddToRegister Word4 Word8
    | SetIndexRegister Word12
    | Display Word4 Word4 Word4
    | End
    deriving (Show, Eq)

pattern ClearScreen' :: Word16
pattern ClearScreen' = 0x00E0

nibble1 :: Word16 -> (Word4, Word12)
nibble1 x =
    (fromIntegral $ x `shiftR` 12, fromIntegral $ x .&. 0x0FFF)

pattern N1 :: Word4 -> Word12 -> Word16
pattern N1 x y <- (nibble1 -> (x, y))

nibble2 :: Word16 -> (Word4, Word4, Word8)
nibble2 (N1 x y) =
    (x, fromIntegral $ y `shiftR` 8, fromIntegral $ y .&. 0x00FF)

pattern N2 :: Word4 -> Word4 -> Word8 -> Word16
pattern N2 x y z <- (nibble2 -> (x, y, z))

nibble3 :: Word16 -> (Word4, Word4, Word4, Word4)
nibble3 (N2 x y z) =
    (x, y, fromIntegral $ z `shiftR` 4, fromIntegral $ z .&. 0x000F)

pattern N3 :: Word4 -> Word4 -> Word4 -> Word4 -> Word16
pattern N3 x y z w <- (nibble3 -> (x, y, z, w))

decode :: Word16 -> Instruction
decode ClearScreen' = ClearScreen
decode (N1 1 nnn) = Jump nnn
decode (N2 6 x nn) = SetRegister x nn
decode (N2 7 x nn) = AddToRegister x nn
decode (N1 0xA nnn) = SetIndexRegister nnn
decode (N3 0xD x y n) = Display x y n
decode _ = End
