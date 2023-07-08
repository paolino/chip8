{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Opcodes (Instruction (..), decode, encode) where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Types (Address, Nibble, Opcode, Byte (..))

data Instruction
    = ClearScreen
    | Jump Address
    | SetRegister Nibble Byte
    | AddToRegister Nibble Byte
    | SetIndexRegister Address
    | Display Nibble Nibble Nibble
    | End
    deriving (Show, Eq)

pattern ClearScreen' :: Opcode
pattern ClearScreen' = 0x00E0

nibble1 :: Opcode -> (Nibble, Address)
nibble1 x =
    (fromIntegral $ x `shiftR` 12, fromIntegral $ x .&. 0x0FFF)

-- >>> N1 0xA 0x28B == 0xA28B
-- True

pattern N1 :: Nibble -> Address -> Opcode
pattern N1 x y <- (nibble1 -> (x, y))
    where
        N1 x y = fromIntegral x `shiftL` 12 .|. fromIntegral y

nibble2 :: Opcode -> (Nibble, Nibble, Word8)
nibble2 (N1 x y) =
    (x, fromIntegral $ y `shiftR` 8, fromIntegral $ y .&. 0x00FF)

-- >>> N2 0xA 0x2 0x8B == 0xA28B
-- True


pattern N2 :: Nibble -> Nibble -> Word8 -> Opcode
pattern N2 x y z <- (nibble2 -> (x, y, z))
    where
        N2 x y z = N1 x (fromIntegral y `shiftL` 8 .|. fromIntegral z)

nibble3 :: Opcode -> (Nibble, Nibble, Nibble, Nibble)
nibble3 (N2 x y z) =
    (x, y, fromIntegral $ z `shiftR` 4, fromIntegral $ z .&. 0x000F)


-- >>> N3 0xA 0x2 0x8 0xB == 0xA28B
-- True

pattern N3 :: Nibble -> Nibble -> Nibble -> Nibble -> Opcode
pattern N3 x y z w <- (nibble3 -> (x, y, z, w))
    where
        N3 x y z w = N2 x y (fromIntegral z `shiftL` 4 .|. fromIntegral w)

decode :: Opcode -> Instruction
decode ClearScreen' = ClearScreen
decode (N1 1 nnn) = Jump nnn
decode (N2 6 x nn) = SetRegister x $ Byte nn
decode (N2 7 x nn) = AddToRegister x $ Byte nn
decode (N1 0xA nnn) = SetIndexRegister nnn
decode (N3 0xD x y n) = Display x y n
decode _ = End

encode :: Instruction -> Opcode
encode ClearScreen = ClearScreen'
encode (Jump nnn) = N1 1 nnn
encode (SetRegister x (Byte nn)) = N2 6 x nn
encode (AddToRegister x (Byte nn)) = N2 7 x nn
encode (SetIndexRegister nnn) = N1 0xA nnn
encode (Display x y n) = N3 0xD x y n
encode End = N1 0xF 0xFFF
