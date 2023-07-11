{-# LANGUAGE PatternSynonyms #-}

module Programs.IBM (program) where

import Control.Monad (void)
import Control.Monad.Free (Free)
import Encoding
    ( AssemblyF
    , List
    , Ref
    , i
    , i_
    , ia_
    , l
    , si
    , sprite
    )
import Opcodes
    ( Instruction (..)
    , pattern V0
    , pattern V1
    )
import Types (Address, Byte)

-- | A line of an IBM sprite is always followed by an empty line
ls :: String -> Free (List [Bool]) ()
ls x = l x >> l (replicate 8 ' ')

-- | Last line of a sprite. We could use 'l' but it would not align with `ls`
le :: String -> Free (List [Bool]) ()
le = l

-- | Place a sprite at the given coordinates, with the given height
-- here we appreciate the power of the assembly language where we can
-- define our own instructions
placeSprite :: Byte -> Byte -> Int -> Ref -> AssemblyF Address
placeSprite x y h s = do
    start <- i $ SetRegister V0 x
    i_ $ SetRegister V1 y
    ia_ s si
    i_ $ Display V0 V1 h
    pure start

-- | Same as 'placeSprite' but without the returned address.
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
    i_ $ Jump start
