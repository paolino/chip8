{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Types
    ( Memory
    , Registers
    , Display
    , Keys
    , GetKeyState (..)
    , GetKey (..)
    , Sprite
    , KeyState (..)
    , Height (..)
    , Nibble (..)
    , Address (..)
    , Opcode (..)
    , Byte (..)
    , Coo (..)
    , pattern VF
    ) where

import Data.Bits (Bits (..))
import Data.Map (Map)
import Data.Word (Word16, Word8)
import Numeric (showHex)

newtype Nibble = Nibble Word8
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

newtype Address = Address Word16
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

newtype Height = Height Word8
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

newtype Opcode = Opcode Word16
    deriving newtype (Eq, Integral, Real, Enum, Num, Ord, Bits)

instance Show Opcode where
    show (Opcode x) = showHex x ""

newtype Byte = Byte Word8
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

type Memory = Map Address Byte

data KeyState = Pressed | Released
    deriving (Eq, Show)

type Keys = Map Nibble KeyState

data GetKey = GetKeyWaiting GetKeyState | GetKeyNotWaiting
    deriving (Show, Eq)

data GetKeyState = GetKeyPressed Nibble | GetKeyReleased Nibble | GetKeyEmpty
    deriving (Show, Eq)

type Registers = Map Nibble Byte

data Coo = Coo Byte Byte
    deriving (Show, Eq, Ord)

type Display = Map Coo Bool

type Sprite = [[Bool]]

pattern VF :: Nibble
pattern VF = 0xF
