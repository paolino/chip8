{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Opcodes
    ( Instruction (..)
    , decode
    , encode
    , opcodeBytes
    , bytesOpcode
    , pattern V0
    , pattern V1
    ) where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Types (Address, Byte (..), Height, Nibble, Opcode)

-- | The instructions that the CPU can execute
data Instruction
    = ClearScreen
    | Jump Address
    | SetRegister Nibble Byte
    | AddToRegister Nibble Byte
    | SetIndexRegister Address
    | Display Nibble Nibble Height
    | End
    deriving (Show, Eq)

pattern ClearScreen' :: Opcode
pattern ClearScreen' = 0x00E0

nibble1 :: Opcode -> (Nibble, Address)
nibble1 x =
    (fromIntegral $ x `shiftR` 12, fromIntegral $ x .&. 0x0FFF)

-- | A pattern synonym for a 1 nibble opcode and a 3 nibble address
-- >>> N1 0xA 0x28B == 0xA28B
-- >>> let N1 x y = N1 0xA 0x28B in x == 0xA && y == 0x28B
pattern N1 :: Nibble -> Address -> Opcode
pattern N1 x y <- (nibble1 -> (x, y))
    where
        N1 x y = fromIntegral x `shiftL` 12 .|. fromIntegral y

nibble2 :: Opcode -> (Nibble, Nibble, Word8)
nibble2 (N1 x y) =
    (x, fromIntegral $ y `shiftR` 8, fromIntegral $ y .&. 0x00FF)

-- | A pattern synonym for a 2 nibble opcode and a byte
-- >>> N2 0xA 0x2 0x8B == 0xA28B
-- >>> let N2 x y z = N2 0xA 0x2 0x8B in x == 0xA && y == 0x2 && z == 0x8B
pattern N2 :: Nibble -> Nibble -> Word8 -> Opcode
pattern N2 x y z <- (nibble2 -> (x, y, z))
    where
        N2 x y z = N1 x (fromIntegral y `shiftL` 8 .|. fromIntegral z)

nibble3 :: Opcode -> (Nibble, Nibble, Nibble, Nibble)
nibble3 (N2 x y z) =
    (x, y, fromIntegral $ z `shiftR` 4, fromIntegral $ z .&. 0x000F)

-- | A pattern synonym for a 4 nibble opcode
-- >>> N3 0xA 0x2 0x8 0xB == 0xA28B
-- >>> let N3 x y z w = N3 0xA 0x2 0x8 0xB in x == 0xA && y == 0x2 && z == 0x8 && w == 0xB
pattern N3 :: Nibble -> Nibble -> Nibble -> Nibble -> Opcode
pattern N3 x y z w <- (nibble3 -> (x, y, z, w))
    where
        N3 x y z w = N2 x y (fromIntegral z `shiftL` 4 .|. fromIntegral w)

-- | Converts an opcode to an instruction
decode :: Opcode -> Instruction
decode ClearScreen' = ClearScreen
decode (N1 1 nnn) = Jump nnn
decode (N2 6 x nn) = SetRegister x $ Byte nn
decode (N2 7 x nn) = AddToRegister x $ Byte nn
decode (N1 0xA nnn) = SetIndexRegister nnn
decode (N3 0xD x y n) = Display x y $ fromIntegral n
decode _ = End

-- | Converts an instruction to an opcode
encode :: Instruction -> Opcode
encode ClearScreen = ClearScreen'
encode (Jump nnn) = N1 1 nnn
encode (SetRegister x (Byte nn)) = N2 6 x nn
encode (AddToRegister x (Byte nn)) = N2 7 x nn
encode (SetIndexRegister nnn) = N1 0xA nnn
encode (Display x y n) = N3 0xD x y $ fromIntegral n
encode End = N1 0xF 0xFFF

-- | Converts a pair of bytes to an opcode
-- >>> bytesOpcode (0x0A, 0x0B) == 0xA0B
-- True
bytesOpcode :: (Byte, Byte) -> Opcode
bytesOpcode (Byte x, Byte y) =
    fromIntegral x `shiftL` 8 .|. fromIntegral y

-- | Converts an opcode to a pair of bytes
-- >>> opcodeBytes 0x2A1B == (0x2A, 0x1B)
opcodeBytes :: Opcode -> (Byte, Byte)
opcodeBytes x = (fromIntegral $ x `shiftR` 8, fromIntegral $ x .&. 0x00FF)

pattern V0 :: Nibble
pattern V0 = 0x0

pattern V1 :: Nibble
pattern V1 = 0x1
