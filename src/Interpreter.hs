{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter (interpret, interpretN) where

import Data.Bits (Bits (shiftL, shiftR, xor, (.&.)), (.|.))
import Data.Foldable (find, foldl')
import Data.List (unfoldr)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Opcodes (Instruction (..), decode)
import State
    ( State (..)
    , pasteSprite
    , readK
    , readM
    , readR
    , readSprite
    , retrieveInstruction
    )
import Types (Byte, Coo (..), KeyState (..), pattern VF)
import Prelude hiding (readFile)

-- | Interpret a single instruction, returning the new state of the CPU, or
-- Nothing if the program has ended
interpret :: State -> Maybe State
interpret State{..} =
    step
        (decode $ retrieveInstruction programCounter memory)
        State{programCounter = programCounter + 2, ..}

step :: Instruction -> State -> Maybe State
step ClearScreen State{..} =
    Just $ State{display = Map.empty, ..}
step (Jump nnn) State{..} =
    Just $ State{programCounter = nnn, ..}
step (SetRegister x nn) State{..} =
    Just $ State{registers = Map.insert x nn registers, ..}
step (AddToRegister x nn) State{..} =
    Just $ State{registers = Map.insertWith (+) x nn registers, ..}
step (SkipIfEq x nn) State{..} =
    Just
        $ if readR x registers == nn
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step (SkipIfNotEq x nn) State{..} =
    Just
        $ if readR x registers /= nn
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step (SkipIfEqR x y) State{..} =
    Just
        $ if readR x registers == readR y registers
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step (SkipIfNotEqR x y) State{..} =
    Just
        $ if readR x registers /= readR y registers
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step (CopyR x y) State{..} =
    Just $ State{registers = Map.insert x (readR y registers) registers, ..}
step (Or x y) State{..} =
    Just $ State{registers = Map.insert x (readR x registers .|. readR y registers) registers, ..}
step (And x y) State{..} =
    Just $ State{registers = Map.insert x (readR x registers .&. readR y registers) registers, ..}
step (Xor x y) State{..} =
    Just $ State{registers = Map.insert x (readR x registers `xor` readR y registers) registers, ..}
step (Add x y) State{..} =
    let x' = readR x registers
        y' = readR y registers
        vf = if y' > 0xFF - x' then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (x' + y') $ registers, ..}
step (Sub x y) State{..} =
    let x' = readR x registers
        y' = readR y registers
        vf = if x' > y' then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (x' - y') $ registers, ..}
step (SubN x y) State{..} =
    let x' = readR x registers
        y' = readR y registers
        vf = if y' > x' then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (y' - x') $ registers, ..}
step (ShiftR x _) State{..} =
    let x' = readR x registers
        vf = if x' .&. 0x1 > 0 then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (shiftR x' 1) $ registers, ..}
step (ShiftL x _) State{..} =
    let x' = readR x registers
        vf = if x' .&. 0x80 > 0 then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (shiftL x' 1) $ registers, ..}
step (Load x) State{..} =
    Just $ State{registers = foldl' (\r (k, y) -> Map.insert k y r) registers [(i, readM (indexRegister + fromIntegral i) memory) | i <- [0 .. x]], ..}
step (Store x) State{..} =
    Just $ State{memory = foldl' (\m (k, y) -> Map.insert k y m) memory [(indexRegister + fromIntegral i, readR i registers) | i <- [0 .. x]], ..}
step (StoreBCD x) State{..} =
    Just $ State{memory = foldl' (\m (k, y) -> Map.insert k y m) memory (zip [0 ..] (bcd $ readR x registers)), ..}
step (AddToIndexRegister x) State{..} =
    Just $ State{indexRegister = indexRegister + fromIntegral (readR x registers), ..}
step (SetDelayTimer x) State{..} =
    Just $ State{delayTimer = readR x registers, ..}
step (LoadDelayTimer x) State{..} =
    Just $ State{registers = Map.insert x delayTimer registers, ..}
step (SkipIfKeyPressed x) State{..} =
    Just
        $ case readK (fromIntegral $ readR x registers) keys of
            Pressed -> State{programCounter = programCounter + 2, ..}
            _ -> State{..}
step (SkipIfNotKeyPressed x) State{..} =
    Just $ case readK (fromIntegral $ readR x registers) keys of
        Released -> State{programCounter = programCounter + 2, ..}
        _ -> State{..}
step (WaitForKey x) State{..} =
    Just $ case find ((==) Pressed . snd) $ Map.assocs keys of
        Nothing -> State{programCounter = programCounter - 2, ..}
        Just (k, _) ->
            State{registers = Map.insert x (fromIntegral k) registers, ..}
step (SetIndexRegister nnn) State{..} =
    Just $ State{indexRegister = nnn, ..}
step (Display x y n) cpu@State{..} =
    let (changed, display') = pasteSprite (Coo x' y') (readSprite n cpu) display
    in  Just
            $ State
                { display = display'
                , registers = Map.insert VF (if changed then 1 else 0) registers
                , ..
                }
  where
    x' = registers Map.! x
    y' = registers Map.! y
step (Call nnn) State{..} =
    Just $ State{stack = programCounter : stack, programCounter = nnn, ..}
step Return State{..} = case stack of
    [] -> Nothing
    x : xs -> Just $ State{stack = xs, programCounter = x, ..}
step End _ = Nothing

-- | Interpret a number of instructions, returning the final state of the CPU, or
-- Nothing if the program has ended
interpretN :: Int -> State -> Maybe State
interpretN 0 cpu = Just cpu
interpretN n cpu = interpret cpu >>= interpretN (n - 1)

bcd :: Byte -> [Byte]
bcd x = reverse $ unfoldr split x
  where
    split 0 = Nothing
    split n = Just $ swap $ divMod n 10
