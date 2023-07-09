{-# LANGUAGE PatternSynonyms #-}

module Programs.IBM (program) where

import Control.Monad.Free (Free)
import Encoding
    ( AssemblyF
    , List
    , Ref
    , i_
    , ia_
    , index
    , l
    , sprite
    , pattern V0
    , pattern V1, jump, i
    )
import Opcodes (Instruction (Display, SetRegister))
import Types (Byte, Address)
import Control.Monad (void)

ls :: String -> Free (List [Bool]) ()
ls x = l x >> l "        "

le :: String -> Free (List [Bool]) ()
le = l

placeSprite :: Byte -> Byte -> Int -> Ref -> AssemblyF Address
placeSprite x y h s = do
    start <- i $ SetRegister V1 x
    i_ $ SetRegister V0 y
    ia_ s index
    i_ $ Display V1 V0 h
    pure start

placeSprite_ :: Byte -> Byte -> Int -> Ref -> AssemblyF ()
placeSprite_ x y h s = void $ placeSprite x y h s

program :: AssemblyF ()
program = do
    i' <- sprite $ do
        ls "████████"
        ls "████████"
        ls "  ████  "
        ls "  ████  "
        ls "  ████  "
        ls "  ████  "
        ls "████████"
        le "████████"
    b'1 <- sprite $ do
        ls "████████"
        ls "████████"
        ls "  ██    "
        ls "  ██████"
        ls "  ██████"
        ls "  ██    "
        ls "████████"
        le "████████"
    b'2 <- sprite $ do
        ls "█       "
        ls "███     "
        ls "███     "
        ls "█       "
        ls "█       "
        ls "███     "
        ls "███     "
        le "█       "
    m'1 <- sprite $ do
        ls "█████   "
        ls "██████  "
        ls "  █████ "
        ls "  ██████"
        ls "  ███ ██"
        ls "  ███  █"
        ls "█████   "
        le "█████   "
    m'2 <- sprite $ do
        ls "█ █     "
        ls "███     "
        ls "███     "
        ls "███     "
        le " █      "
    m'3 <- sprite $ do
        ls "   █████"
        ls "  ██████"
        ls " █████  "
        ls "██████  "
        ls "██ ███  "
        ls "█  ███  "
        ls "   █████"
        le "   █████"

    start <- placeSprite 12 9 15 i'
    placeSprite_ 21 9 15 b'1
    placeSprite_ 29 9 15 b'2
    placeSprite_ 33 9 15 m'1
    placeSprite_ 41 15 9 m'2
    placeSprite_ 44 9 15 m'3
    i_ $ jump start
