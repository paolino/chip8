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
    , decreaseTimers
    , pasteSprite
    , readK
    , readM
    , readR
    , readSprite
    , retrieveInstruction
    )
import System.Random (Random (randomR), StdGen, split)
import Types (Byte (..), Coo (..), KeyState (..), pattern VF)
import Prelude hiding (readFile)
import Data.IntMap (findWithDefault)

-- | Interpret a single instruction, returning the new state of the CPU, or
-- Nothing if the program has ended
interpret :: StdGen -> State -> Maybe State
interpret g State{..} =
    step
        g
        (decode $ retrieveInstruction programCounter memory)
        $ decreaseTimers
        $ State{programCounter = programCounter + 2, ..}

step :: StdGen -> Instruction -> State -> Maybe State
step _ ClearScreen State{..} =
    Just $ State{display = Map.empty, ..}
step _ (Jump nnn) State{..} =
    Just $ State{programCounter = nnn, ..}
step _ (JumpV0 nnn) State{..} =
    Just $ State{programCounter = nnn + fromIntegral (readR 0 registers), ..}
step _ (SetRegister x nn) State{..} =
    Just $ State{registers = Map.insert x nn registers, ..}
step _ (AddToRegister x nn) State{..} =
    Just $ State{registers = Map.insertWith (+) x nn registers, ..}
step _ (SkipIfEq x nn) State{..} =
    Just
        $ if readR x registers == nn
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step _ (SkipIfNotEq x nn) State{..} =
    Just
        $ if readR x registers /= nn
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step _ (SkipIfEqR x y) State{..} =
    Just
        $ if readR x registers == readR y registers
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step _ (SkipIfNotEqR x y) State{..} =
    Just
        $ if readR x registers /= readR y registers
            then State{programCounter = programCounter + 2, ..}
            else State{..}
step _ (CopyR x y) State{..} =
    Just $ State{registers = Map.insert x (readR y registers) registers, ..}
step _ (Or x y) State{..} =
    Just $ State{registers = Map.insert x (readR x registers .|. readR y registers) registers, ..}
step _ (And x y) State{..} =
    Just $ State{registers = Map.insert x (readR x registers .&. readR y registers) registers, ..}
step _ (Xor x y) State{..} =
    Just $ State{registers = Map.insert x (readR x registers `xor` readR y registers) registers, ..}
step _ (Add x y) State{..} =
    let x' = readR x registers
        y' = readR y registers
        vf = if y' > 0xFF - x' then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (x' + y') $ registers, ..}
step _ (Sub x y) State{..} =
    let x' = readR x registers
        y' = readR y registers
        vf = if x' > y' then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (x' - y') $ registers, ..}
step _ (SubN x y) State{..} =
    let x' = readR x registers
        y' = readR y registers
        vf = if y' > x' then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (y' - x') $ registers, ..}
step _ (ShiftR x _) State{..} =
    let x' = readR x registers
        vf = if x' .&. 0x1 > 0 then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (shiftR x' 1) $ registers, ..}
step _ (ShiftL x _) State{..} =
    let x' = readR x registers
        vf = if x' .&. 0x80 > 0 then 0x1 else 0x0
    in  Just $ State{registers = Map.insert VF vf . Map.insert x (shiftL x' 1) $ registers, ..}
step _ (Load x) State{..} =
    Just $ State{registers = foldl' (\r (k, y) -> Map.insert k y r) registers [(i, readM (indexRegister + fromIntegral i) memory) | i <- [0 .. x]], ..}
step _ (Store x) State{..} =
    Just $ State{memory = foldl' (\m (k, y) -> Map.insert k y m) memory [(indexRegister + fromIntegral i, readR i registers) | i <- [0 .. x]], ..}
step _ (StoreBCD x) State{..} =
    Just $ State{memory = foldl' (\m (k, y) -> Map.insert k y m) memory (zip [0 ..] (bcd $ readR x registers)), ..}
step _ (AddToIndexRegister x) State{..} =
    Just $ State{indexRegister = indexRegister + fromIntegral (readR x registers), ..}
step _ (SetDelayTimer x) State{..} =
    Just $ State{delayTimer = readR x registers, ..}
step _ (LoadDelayTimer x) State{..} =
    Just $ State{registers = Map.insert x delayTimer registers, ..}
step _ (SkipIfKeyPressed x) State{..} =
    Just
        $ case readK (fromIntegral $ readR x registers) keys of
            Pressed -> State{programCounter = programCounter + 2, ..}
            _ -> State{..}
step _ (SkipIfNotKeyPressed x) State{..} =
    Just $ case readK (fromIntegral $ readR x registers) keys of
        Released -> State{programCounter = programCounter + 2, ..}
        _ -> State{..}
step _ (WaitForKey x) State{..} =
    Just $ case find ((==) Pressed . snd) $ Map.assocs keys of
        Nothing -> State{programCounter = programCounter - 2, ..}
        Just (k, _) ->
            State{registers = Map.insert x (fromIntegral k) registers, ..}
step _ (SetIndexRegister nnn) State{..} =
    Just $ State{indexRegister = nnn, ..}
step _ (Display x y n) cpu@State{..} =
    let (changed, display') = pasteSprite (Coo x' y') (readSprite n cpu) display
    in  Just
            $ State
                { display = display'
                , registers = Map.insert VF (if changed then 1 else 0) registers
                , ..
                }
  where
    x' = Map.findWithDefault 0 x registers
    y' = Map.findWithDefault 0 y registers
step _ (Call nnn) State{..} =
    Just $ State{stack = programCounter : stack, programCounter = nnn, ..}
step _ Return State{..} = case stack of
    [] -> Nothing
    x : xs -> Just $ State{stack = xs, programCounter = x, ..}
step g (Random x nn) State{..} =
    let (r, _g') = randomR (0, 255) g
    in  Just $ State{registers = Map.insert x (Byte r .&. nn) registers, ..}
step _ End _ = Nothing

-- | Interpret a number of instructions, returning the final state of the CPU, or
-- Nothing if the program has ended
interpretN :: StdGen -> Int -> State -> Maybe State
interpretN _ 0 cpu = Just cpu
interpretN g n cpu =
    let (g', g'') = split g
    in  interpret g' cpu >>= interpretN g'' (n - 1)

bcd :: Byte -> [Byte]
bcd x = reverse $ unfoldr split' x
  where
    split' 0 = Nothing
    split' n = Just $ swap $ divMod n 10
