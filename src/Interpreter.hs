{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter (interpret) where

import Data.Functor ((<&>))
import Data.Map qualified as Map
import Opcodes (Instruction (..), decode)
import State
    ( State (..)
    , pasteSprite
    , readSprite
    , retrieveInstruction
    )
import Types (Coo (..), pattern VF)
import Prelude hiding (readFile)

interpret :: State -> Maybe State
interpret State{..} =
    step (decode $ retrieveInstruction programCounter memory) State{..}
        <&> \cpu'' -> cpu''{programCounter = programCounter + 2}

step :: Instruction -> State -> Maybe State
step ClearScreen State{..} =
    Just $ State{display = Map.empty, ..}
step (Jump nnn) State{..} =
    Just $ State{programCounter = nnn, ..}
step (SetRegister x nn) State{..} =
    Just $ State{registers = Map.insert x nn registers, ..}
step (AddToRegister x nn) State{..} =
    Just $ State{registers = Map.insertWith (+) x nn registers, ..}
step (SetIndexRegister nnn) State{..} =
    Just $ State{indexRegister = nnn, ..}
step (Display x y n) cpu@State{..} =
    let (changed, display') = pasteSprite (Coo x' y') (readSprite n cpu) display
     in Just
            $ State
                { display = display'
                , registers =
                    Map.insert VF (if changed then 1 else 0) registers
                , ..
                }
  where
    x' = registers Map.! x
    y' = registers Map.! y
step End _ = Nothing
