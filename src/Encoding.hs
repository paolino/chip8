{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, PatternSynonyms #-}

module Encoding where

import Control.Lens (Lens', lens)
import Control.Monad (void)
import Control.Monad.Free (Free (..), MonadFree, liftF)
import Data.Bifunctor (second)
import Data.Bits (Bits (shiftL, (.|.)))
import Data.ByteString (ByteString, pack)
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import Graphics (game)
import Offset (memoryOffset)
import Opcodes (Instruction (..), bytesOpcode, decode, encode, opcodeBytes)
import State (bootState)
import Terminal.Game (playGame)
import Text.Pretty.Simple (pPrint)
import Types (Address, Byte (..), Memory, Sprite, Nibble)

newtype Ref = Ref Int
    deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

data Assembly a where
    StoreSprite :: Sprite -> (Ref -> a) -> Assembly a
    StoreInstruction :: Instruction -> (Address -> a) -> Assembly a
    StoreAddressInstruction :: Ref -> (Address -> Instruction) -> (Address -> a) -> Assembly a

deriving instance Functor Assembly

type AssemblyF = Free Assembly

addSprite :: (MonadFree Assembly m) => Sprite -> m Ref
addSprite sprite' = liftF $ StoreSprite sprite' id

addInstruction :: (MonadFree Assembly m) => Instruction -> m Address
addInstruction instruction =
    liftF $ StoreInstruction instruction id

addInstruction_ :: (MonadFree Assembly m) => Instruction -> m ()
addInstruction_ instruction = void $ addInstruction instruction

addAddressInstruction
    :: (MonadFree Assembly m) => Ref -> (Address -> Instruction) -> m Address
addAddressInstruction ref f = liftF $ StoreAddressInstruction ref f id

addAddressInstruction_
    :: (MonadFree Assembly m) => Ref -> (Address -> Instruction) -> m ()
addAddressInstruction_ ref f = void $ addAddressInstruction ref f

data Encoding = Encoding
    { encodingMemory :: Memory
    , encodingTop :: Address
    , encodingAddressTop :: Ref
    , encodingAddresses :: Map Ref [(Address, Address -> Instruction)]
    , encodingSprites :: Map Ref Sprite
    }

splitIn :: Int -> [a] -> [(a, a)]
splitIn _ [] = []
splitIn _ [_] = []
splitIn n (x : y : xs) = (x, y) : splitIn n xs

showCode :: Memory -> Address -> [Instruction]
showCode memory top = fmap (decode . bytesOpcode) . splitIn 2 . take (fromIntegral n) $ toList memory
  where
    n = top - memoryOffset

instance Show Encoding where
    show Encoding{..} =
        show
            ( showCode encodingMemory encodingTop
            , encodingTop
            , encodingAddressTop
            , second (fmap $ fmap ($ 0)) <$> Map.assocs encodingAddresses
            , encodingSprites
            )

encodingMemoryL :: Lens' Encoding Memory
encodingMemoryL = lens encodingMemory $ \e m -> e{encodingMemory = m}

encodingTopL :: Lens' Encoding Address
encodingTopL = lens encodingTop $ \e t -> e{encodingTop = t}

encodingAddressTopL :: Lens' Encoding Ref
encodingAddressTopL =
    lens encodingAddressTop $ \e t -> e{encodingAddressTop = t}

encodingAddressesL :: Lens' Encoding (Map Ref [(Address, Address -> Instruction)])
encodingAddressesL = lens encodingAddresses $ \e t -> e{encodingAddresses = t}

interpreter :: AssemblyF a -> Encoding -> Encoding
interpreter (Pure _) encoding =
    foldl' solveRef encoding $ Map.assocs $ encodingSprites encoding
  where
    solveRef :: Encoding -> (Ref, Sprite) -> Encoding
    solveRef encoding' (ref, sprite') =
        foldl' g encoding''
            $ Map.findWithDefault [] ref
            $ encodingAddresses encoding'
      where
        encoding'' =
            encoding'
                { encodingMemory =
                    storeSprite sprite' (encodingTop encoding')
                        $ encodingMemory encoding'
                , encodingTop =
                    encodingTop encoding'
                        + fromIntegral (length sprite')
                }
        g encoding''' (targetAddress, solveInstruction) =
            encoding'''
                { encodingMemory =
                    storeInstruction targetAddress (solveInstruction address)
                        $ encodingMemory encoding'''
                }
          where
            address = encodingTop encoding'
interpreter (Free (StoreSprite sprite' f)) encoding =
    let (ref, encoding') = refSprite encoding sprite'
     in interpreter (f ref) encoding'
interpreter (Free (StoreInstruction instruction f)) encoding =
    let (address, encoding') = pushInstruction encoding instruction
     in interpreter (f address) encoding'
interpreter (Free (StoreAddressInstruction ref f g)) encoding =
    let (address, encoding') = refInstruction ref encoding f
     in interpreter (g address) encoding'

refInstruction :: Ref -> Encoding -> (Address -> Instruction) -> (Address, Encoding)
refInstruction ref encoding f =
    let address = encodingTop encoding
        encoding' =
            encoding
                { encodingTop = address + 2
                , encodingAddresses =
                    Map.insertWith (<>) ref [(address, f)]
                        $ encodingAddresses encoding
                }
     in (address, encoding')

refSprite :: Encoding -> Sprite -> (Ref, Encoding)
refSprite encoding sprite' =
    let ref = encodingAddressTop encoding
        encoding' =
            encoding
                { encodingAddressTop = encodingAddressTop encoding + 1
                , encodingSprites =
                    Map.insert ref sprite' $ encodingSprites encoding
                }
     in (ref, encoding')

pushInstruction :: Encoding -> Instruction -> (Address, Encoding)
pushInstruction encoding instruction =
    let address = encodingTop encoding
        encoding' =
            encoding
                { encodingTop = encodingTop encoding + 2
                , encodingMemory =
                    storeInstruction address instruction
                        $ encodingMemory encoding
                }
     in (address, encoding')

storeSprite :: Sprite -> Address -> Memory -> Memory
storeSprite sprite' address memory =
    foldl' storeLine memory (zip [address ..] sprite')
  where
    storeLine memory' (address', line') =
        Map.insert address' (byteOf line') memory'
    byteOf = foldl' f 0
      where
        f acc True = acc `shiftL` 1 .|. 1
        f acc False = acc `shiftL` 1

offloadMemory :: Memory -> ByteString
offloadMemory = pack . stream memoryOffset . Map.assocs
  where
    stream :: Address -> [(Address, Byte)] -> [Word8]
    stream _ [] = []
    stream t ((a, Byte b) : xs) =
        [0 | _ <- [(t + 1) .. a]]
            <> [b]
            <> stream (a + 1) xs

storeInstruction :: Address -> Instruction -> Memory -> Memory
storeInstruction address instruction =
    case opcodeBytes $ encode instruction of
        (h, l') -> Map.insert address h . Map.insert (address + 1) l'

line :: String -> [Bool]
line = fmap (/= ' ')

data List x a where
    Elem :: x -> a -> List x a
    deriving (Functor)

list :: Free (List x) a -> [x]
list (Pure _) = []
list (Free (Elem x f)) = x : list f

el :: x -> Free (List x) ()
el x = liftF $ Elem x ()

l :: String -> Free (List [Bool]) ()
l = el . line

sprite :: Free (List [Bool]) a -> Free Assembly Ref
sprite = addSprite . list

i_ :: Instruction -> Free Assembly ()
i_ = addInstruction_

ia_ :: Ref -> (Address -> Instruction) -> Free Assembly ()
ia_ = addAddressInstruction_

index :: Address -> Instruction
index = SetIndexRegister

jump :: Address -> Instruction
jump = Jump

-- write 2 8x8 sprites on the screen,
-- a circle at (1, 1) and a cross at (8, 1)
example :: AssemblyF ()
example = do
    s1 <- sprite $ do
        l "  xxxx  "
        l " x    x "
        l "x      x"
        l "x      x"
        l "x      x"
        l "x      x"
        l " x    x "
        l "  xxxx  "
    s2 <- sprite $ do
        l "x      x"
        l " x    x "
        l "  x  x  "
        l "   xx   "
        l "   xx   "
        l "  x  x  "
        l " x    x "
        l "x      x"
    i_ ClearScreen
    -- set the x any y coordinates of the circle
    i_ $ SetRegister 0 1
    -- point to the circle sprite
    ia_ s1 index
    -- draw the circle
    i_ $ Display 0 0 8
    -- set the y coordinate of the cross
    i_ $ SetRegister 1 9
    -- point to the cross sprite
    ia_ s2 index
    -- draw the cross
    i_ $ Display 0 1 8

-- >>> interpreter example $ Encoding mempty 0 0 mempty mempty

exampleProgram :: ByteString
exampleProgram =
    offloadMemory
        $ encodingMemory
        $ interpreter example
        $ Encoding mempty memoryOffset 0 mempty mempty

testProgram :: IO ()
testProgram = playGame . game . bootState $ exampleProgram

testInterpreter :: IO ()
testInterpreter =
    pPrint
        $ interpreter example
        $ Encoding mempty memoryOffset 0 mempty mempty

pattern V0 :: Nibble
pattern V0 = 0x0

pattern V1 :: Nibble
pattern V1 = 0x1
