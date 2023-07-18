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
    , pattern V2
    , pattern V3
    , pattern V4
    ) where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Types (Address, Byte (..), Height, Nibble, Opcode)

-- | The instructions that the CPU can execute
data Instruction
    = ClearScreen
    | Jump Address
    | JumpV0 Address
    | SetRegister Nibble Byte
    | AddToRegister Nibble Byte
    | SkipIfEq Nibble Byte
    | SkipIfNotEq Nibble Byte
    | SkipIfEqR Nibble Nibble
    | SkipIfNotEqR Nibble Nibble
    | CopyR Nibble Nibble
    | Or Nibble Nibble
    | And Nibble Nibble
    | Xor Nibble Nibble
    | Add Nibble Nibble
    | Sub Nibble Nibble
    | SubN Nibble Nibble
    | ShiftR Nibble Nibble
    | ShiftL Nibble Nibble
    | Load Nibble
    | Store Nibble
    | StoreBCD Nibble
    | AddToIndexRegister Nibble
    | SetIndexRegister Address
    | SetIndexRegisterToHexSprite Nibble
    | SetDelayTimer Nibble
    | SetSoundTimer Nibble
    | LoadDelayTimer Nibble
    | SkipIfKeyPressed Nibble
    | SkipIfNotKeyPressed Nibble
    | WaitForKey Nibble
    | Display Nibble Nibble Height
    | Call Address
    | Return
    | Random Nibble Byte
    | Sys Address
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
decode (N1 0x1 nnn) = Jump nnn
decode (N1 0xB nnn) = JumpV0 nnn
decode (N2 0x6 x nn) = SetRegister x $ Byte nn
decode (N2 0x7 x nn) = AddToRegister x $ Byte nn
decode (N2 0x3 x nn) = SkipIfEq x $ Byte nn
decode (N2 0x4 x nn) = SkipIfNotEq x $ Byte nn
decode (N3 0x5 x y 0x0) = SkipIfEqR x y
decode (N3 0x9 x y 0x0) = SkipIfNotEqR x y
decode (N3 0x8 x y 0x0) = CopyR x y
decode (N3 0x8 x y 0x1) = Or x y
decode (N3 0x8 x y 0x2) = And x y
decode (N3 0x8 x y 0x3) = Xor x y
decode (N3 0x8 x y 0x4) = Add x y
decode (N3 0x8 x y 0x5) = Sub x y
decode (N3 0x8 x y 0x7) = SubN x y
decode (N3 0x8 x y 0x6) = ShiftR x y
decode (N3 0x8 x y 0xE) = ShiftL x y
decode (N3 0xF x 0x6 0x5) = Load x
decode (N3 0xF x 0x5 0x5) = Store x
decode (N3 0xF x 0x3 0x3) = StoreBCD x
decode (N3 0xF x 0x1 0xE) = AddToIndexRegister x
decode (N3 0xF x 0x1 0x5) = SetDelayTimer x
decode (N3 0xF x 0x0 0x7) = LoadDelayTimer x
decode (N3 0xF x 0x1 0x8) = SetSoundTimer x
decode (N3 0xE x 0x9 0xE) = SkipIfKeyPressed x
decode (N3 0xE x 0xA 0x1) = SkipIfNotKeyPressed x
decode (N3 0xF x 0x0 0xA) = WaitForKey x
decode (N3 0xF x 0x2 0x9) = SetIndexRegisterToHexSprite x
decode (N1 0xA nnn) = SetIndexRegister nnn
decode (N3 0xD x y n) = Display x y $ fromIntegral n
decode (N1 0x2 nnn) = Call nnn
decode (N3 0x0 0x0 0xE 0xE) = Return
decode (N2 0xC x nn) = Random x $ Byte nn
decode (N1 0x0 nnn) = Sys nnn
decode _ = End

-- | Converts an instruction to an opcode
encode :: Instruction -> Opcode
encode ClearScreen = ClearScreen'
encode (Jump nnn) = N1 0x1 nnn
encode (JumpV0 nnn) = N1 0xB nnn
encode (SetRegister x (Byte nn)) = N2 0x6 x nn
encode (AddToRegister x (Byte nn)) = N2 0x7 x nn
encode (SkipIfEq x (Byte nn)) = N2 0x3 x nn
encode (SkipIfNotEq x (Byte nn)) = N2 0x4 x nn
encode (SkipIfEqR x y) = N3 0x5 x y 0x0
encode (SkipIfNotEqR x y) = N3 0x9 x y 0x0
encode (CopyR x y) = N3 0x8 x y 0x0
encode (Or x y) = N3 0x8 x y 0x1
encode (And x y) = N3 0x8 x y 0x2
encode (Xor x y) = N3 0x8 x y 0x3
encode (Add x y) = N3 0x8 x y 0x4
encode (Sub x y) = N3 0x8 x y 0x5
encode (SubN x y) = N3 0x8 x y 0x7
encode (ShiftR x y) = N3 0x8 x y 0x6
encode (ShiftL x y) = N3 0x8 x y 0xE
encode (Load x) = N3 0xF x 0x6 0x5
encode (Store x) = N3 0xF x 0x5 0x5
encode (StoreBCD x) = N3 0xF x 0x3 0x3
encode (AddToIndexRegister x) = N3 0xF x 0x1 0xE
encode (SetDelayTimer x) = N3 0xF x 0x1 0x5
encode (LoadDelayTimer x) = N3 0xF x 0x0 0x7
encode (SetSoundTimer x) = N3 0xF x 0x1 0x8
encode (SkipIfKeyPressed x) = N3 0xE x 0x9 0xE
encode (SkipIfNotKeyPressed x) = N3 0xE x 0xA 0x1
encode (WaitForKey x) = N3 0xF x 0x0 0xA
encode (SetIndexRegisterToHexSprite x) = N3 0xF x 0x2 0x9
encode (SetIndexRegister nnn) = N1 0xA nnn
encode (Display x y n) = N3 0xD x y $ fromIntegral n
encode (Call nnn) = N1 0x2 nnn
encode Return = N3 0x0 0x0 0xE 0xE
encode (Random x (Byte nn)) = N2 0xC x nn
encode (Sys nnn) = N1 0x0 nnn
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

pattern V2 :: Nibble
pattern V2 = 0x2

pattern V3 :: Nibble
pattern V3 = 0x3

pattern V4 :: Nibble
pattern V4 = 0x4
