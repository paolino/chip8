{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Types
    ( Memory
    , Registers
    , Display
    , Sprite
    , Nibble
    , Address
    , Opcode (..)
    , Byte (..)
    , Coo (..)
    , pattern VF
    ) where

import Data.Bits (Bits (..))
import Data.Map (Map)
import Data.Word (Word16, Word8)

newtype Nibble = Nibble Word8
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

newtype Address = Address Word16
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

newtype Opcode = Opcode Word16
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

newtype Byte = Byte Word8
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

type Memory = Map Address Byte

type Registers = Map Nibble Byte

data Coo = Coo Byte Byte
    deriving (Show, Eq, Ord)

type Display = Map Coo Bool

type Sprite = [[Bool]]

pattern VF :: Nibble
pattern VF = 0xF