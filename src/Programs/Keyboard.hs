{-# LANGUAGE PatternSynonyms #-}

module Programs.Keyboard where

import Encoding
    ( AssemblyF
    , Ref
    , addRoutine
    , i_
    , ia_
    , nextAddress
    )
import Opcodes
    ( Instruction (..)
    , pattern V0
    , pattern V1
    , pattern V2
    )
import Types (Byte, Nibble)

loop :: AssemblyF () -> AssemblyF ()
loop w = do
    a <- nextAddress
    w
    i_ . Jump $ a

ite1 :: Nibble -> Byte -> Ref -> AssemblyF () -> AssemblyF ()
ite1 v x r w = do
    i_ $ SkipIfEq v x
    ia_ r Jump
    w

endR :: AssemblyF Ref
endR = addRoutine $ loop $ pure ()

program :: AssemblyF ()
program = do
    let display = i_ $ Display V0 V1 5
        loadHex = i_ $ SetIndexRegisterToHexSprite V2

    i_ $ SetRegister V0 0x0
    i_ $ SetRegister V1 0x0
    i_ $ SetRegister V2 0x0
    loadHex
    display
    end <- endR
    loop $ do
        i_ $ WaitForKey V2
        ite1 V2 0xF end $ do
            display
            loadHex
            display