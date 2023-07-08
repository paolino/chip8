{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (Memory, Registers, Display, Sprite, Word4, Word12) where

import Data.Bits (Bits (..))
import Data.Map (Map)
import Data.Word (Word16, Word8)

newtype Word4 = Word4 Word8
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)
newtype Word12 = Word12 Word16
    deriving newtype (Show, Eq, Integral, Real, Enum, Num, Ord, Bits)

type Memory = Map Word12 Word8

type Registers = Map Word4 Word8

type Display = Map (Word8, Word8) Bool

type Sprite = [[Bool]]
