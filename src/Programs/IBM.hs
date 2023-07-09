{-# LANGUAGE PatternSynonyms #-}

module Programs.IBM (program) where

import Control.Monad.Free (Free)
import Encoding (AssemblyF, List, i_, ia_, index, l, s, pattern V1, pattern V0 ,Ref)
import Opcodes (Instruction (Display, SetRegister))
import Types (Byte)

ls :: String -> Free (List [Bool]) ()
ls x = l x >> l "        "

le :: String -> Free (List [Bool]) ()
le = l

placeSprite :: Byte -> Byte -> Int -> Ref -> AssemblyF ()
placeSprite x y h sprite = do
    i_ $ SetRegister V1 x
    i_ $ SetRegister V0 y
    ia_ sprite index
    i_ $ Display V1 V0 h

program :: AssemblyF ()
program = do
    i' <- s $ do
        ls "████████"
        ls "████████"
        ls "  ████  "
        ls "  ████  "
        ls "  ████  "
        ls "  ████  "
        ls "████████"
        le "████████"
    b'1 <- s $ do
        ls "████████"
        ls "████████"
        ls "  ██    "
        ls "  ██████"
        ls "  ██████"
        ls "  ██    "
        ls "████████"
        le "████████"
    b'2 <- s $ do
        ls "█       "
        ls "███     "
        ls "███     "
        ls "█       "
        ls "█       "
        ls "███     "
        ls "███     "
        le "█       "
    m'1 <- s $ do
        ls "█████   "
        ls "██████  "
        ls "  █████ "
        ls "  ██████"
        ls "  ███ ██"
        ls "  ███  █"
        ls "█████   "
        le "█████   "
    m'2 <- s $ do
        ls "█ █     "
        ls "███     "
        ls "███     "
        ls "███     "
        le " █      "
    m'3 <- s $ do
        ls "   █████"
        ls "  ██████"
        ls " █████  "
        ls "██████  "
        ls "██ ███  "
        ls "█  ███  "
        ls "   █████"
        le "   █████"

    placeSprite 12 9 15 i'
    placeSprite 21 9 15 b'1
    placeSprite 29 9 15 b'2
    placeSprite 33 9 15 m'1
    placeSprite 41 15 9 m'2
    placeSprite 44 9 15 m'3
