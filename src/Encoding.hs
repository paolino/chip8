{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Encoding
    ( -- * types
      AssemblyF
    , Assembly
    , Encoding (..)
    , Ref (..)

      -- * lenses
    , encodingMemoryL
    , encodingTopL
    , encodingRefTopL
    , encodingReferencesL

      -- * functions
    , encodingBoot
    , interpreter
    , offloadMemory
    , showCode

      -- * instructions
    , addSprite
    , addInstruction
    , addInstruction_
    , addAddressInstruction
    , addAddressInstruction_
    , addRoutine

      -- * sprites
    , sprite

      -- * example
    , exampleProgram

      -- * combinators
    , List (..)
    , list
    , el

      -- * sprite definition support
    , line
    , l

      -- * instruction definition support
    , i
    , i_
    , ia_
    , si
    , ls
    , le
    , ia
    , routine
    , ret
    , call
    )
where

import Control.Lens (Lens', lens)
import Control.Monad (void)
import Control.Monad.Free (Free (..), MonadFree, liftF)
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Bits (Bits (shiftL, (.|.)))
import Data.ByteString (ByteString, pack)
import Data.Either (lefts, partitionEithers, rights)
import Data.Foldable (foldl', toList)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import Debug.Trace
import Offset (memoryOffset)
import Opcodes (Instruction (..), bytesOpcode, decode, encode, opcodeBytes)
import Types (Address, Byte (..), Memory, Sprite)

-- | A reference to a sprite.
newtype Ref = Ref Int
    deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

-- | A DSL for assembling a program.
data Assembly a where
    -- | Store a sprite in memory, returning a reference to it.
    StoreSprite
        :: Sprite -> (Ref -> a) -> Assembly a
    -- | Store an instruction in memory, returning the address of it.
    StoreInstruction
        :: Instruction -> (Address -> a) -> Assembly a
    -- | Store an instruction that references a sprite in memory, returning the address of it.
    -- The address of the sprite will be passed to the function that generates the instruction.
    StoreAddressInstruction
        :: Ref -> (Address -> Instruction) -> (Address -> a) -> Assembly a
    -- | Add an assembly routine to the program, returning a reference to it.
    AddRoutine :: AssemblyF () -> (Ref -> a) -> Assembly a

deriving instance Functor Assembly

-- | The free monad of the assembly DSL.
type AssemblyF = Free Assembly

-- | Add a sprite to the program.
addSprite :: (MonadFree Assembly m) => Sprite -> m Ref
addSprite sprite' = liftF $ StoreSprite sprite' id

-- | Add an instruction to the program.
addInstruction :: (MonadFree Assembly m) => Instruction -> m Address
addInstruction instruction =
    liftF $ StoreInstruction instruction id

-- | Add an instruction to the program, discarding the address.
addInstruction_ :: (MonadFree Assembly m) => Instruction -> m ()
addInstruction_ instruction = void $ addInstruction instruction

-- | Add an instruction that references a sprite to the program.
addAddressInstruction
    :: (MonadFree Assembly m) => Ref -> (Address -> Instruction) -> m Address
addAddressInstruction ref f = liftF $ StoreAddressInstruction ref f id

-- | Add an instruction that references a sprite to the program, discarding the address.
addAddressInstruction_
    :: (MonadFree Assembly m) => Ref -> (Address -> Instruction) -> m ()
addAddressInstruction_ ref f = void $ addAddressInstruction ref f

-- | Add an assembly to the program.
addRoutine :: (MonadFree Assembly m) => AssemblyF () -> m Ref
addRoutine f = liftF $ AddRoutine f id

-- | The state of the assembler.
data Encoding = Encoding
    { encodingMemory :: Memory
    -- ^ The memory of the encoding.
    , encodingLastAddress :: Address
    -- ^ The last used address in the memory. Computable, but we track it anyway
    , encodingLastReference :: Ref
    -- ^ The last used reference. Computable, but we track it anyway
    , encodingReferences :: Map Ref [(Address, Address -> Instruction)]
    -- ^ The references of the encoding. Each reference is associated with a list of
    -- addresses and functions that generate instructions that reference the sprite
    , encodingTargets :: Map Ref (Either Sprite (AssemblyF ()))
    -- ^ The targets of the encoding. Each reference is associated with a sprite or an
    -- assembly routine.
    , routineAddresses :: Map Ref Address
    }

splitIn :: Int -> [a] -> [(a, a)]
splitIn _ [] = []
splitIn _ [_] = []
splitIn n (x : y : xs) = (x, y) : splitIn n xs

-- | Show the code in memory as a list of instructions.
showCode :: Memory -> Address -> [Instruction]
showCode memory top =
    fmap (decode . bytesOpcode)
        . splitIn 2
        . take (fromIntegral n)
        $ toList memory
  where
    n = top - memoryOffset

instance Show Encoding where
    show Encoding{..} =
        show
            ( showCode encodingMemory encodingLastAddress
            , encodingLastAddress
            , encodingLastReference
            , second (fmap $ fmap ($ 0)) <$> Map.assocs encodingReferences
            , lefts $ (\(k, v) -> bimap (k,) (k,) v) <$> Map.assocs encodingTargets
            , rights $ (\(k, v) -> v $> k) <$> Map.assocs encodingTargets
            )

spritesAndRoutines :: Encoding -> ([(Ref, Sprite)], [(Ref, AssemblyF ())])
spritesAndRoutines e =
    partitionEithers $ (\(k, v) -> bimap (k,) (k,) v) <$> Map.assocs (encodingTargets e)

-- | A lens over the memory of the encoding.
encodingMemoryL :: Lens' Encoding Memory
encodingMemoryL = lens encodingMemory $ \e m -> e{encodingMemory = m}

-- | A lens over the top of the encoding.
encodingTopL :: Lens' Encoding Address
encodingTopL = lens encodingLastAddress $ \e t -> e{encodingLastAddress = t}

-- | A lens over the sprite references top of the encoding.
encodingRefTopL :: Lens' Encoding Ref
encodingRefTopL =
    lens encodingLastReference $ \e t -> e{encodingLastReference = t}

-- | A lens over the sprite references of the encoding.
encodingReferencesL :: Lens' Encoding (Map Ref [(Address, Address -> Instruction)])
encodingReferencesL = lens encodingReferences $ \e t -> e{encodingReferences = t}

-- | Interpret an assembly program into a change of state.
interpreter :: AssemblyF a -> Encoding -> Encoding
interpreter (Pure _) encoding =
    solveRoutines routines $ foldl' solveSprites encoding sprites
  where
    solveRoutines :: [(Ref, AssemblyF ())] -> Encoding -> Encoding
    solveRoutines xs e =
        let
            pass0 = foldl' g e xs
            g :: Encoding -> (Ref, AssemblyF ()) -> Encoding
            g e' (ref, routine) =
                interpreter routine
                    $ e'
                        { routineAddresses = Map.insert ref (encodingLastAddress e') $ routineAddresses e'
                        , encodingTargets = Map.delete ref $ encodingTargets e'
                        }
            pass1 = foldl' r pass0 $ Map.assocs $ routineAddresses pass0
            r :: Encoding -> (Ref, Address) -> Encoding
            r e' (ref, address) = foldl' h e' $ Map.findWithDefault [] ref $ encodingReferences e'
              where
                h e'' (targetAddress, solveInstruction) =
                    e''
                        { encodingMemory =
                            storeInstruction targetAddress (solveInstruction address)
                                $ encodingMemory e''
                        }
        in
            pass1
    (sprites, routines') = spritesAndRoutines encoding
    routines = traceShow (length routines') routines'

    solveSprites :: Encoding -> (Ref, Sprite) -> Encoding
    solveSprites encoding' (ref, sprite') =
        foldl' g encoding''
            $ Map.findWithDefault [] ref
            $ encodingReferences encoding'
      where
        encoding'' =
            encoding'
                { encodingMemory =
                    storeSprite sprite' (encodingLastAddress encoding')
                        $ encodingMemory encoding'
                , encodingLastAddress =
                    encodingLastAddress encoding'
                        + fromIntegral (length sprite')
                }
        g encoding''' (targetAddress, solveInstruction) =
            encoding'''
                { encodingMemory =
                    storeInstruction targetAddress (solveInstruction address)
                        $ encodingMemory encoding'''
                }
          where
            address = encodingLastAddress encoding'
interpreter (Free (StoreSprite sprite' f)) encoding =
    let (ref, encoding') = pushRefSprite encoding sprite'
    in  interpreter (f ref) encoding'
interpreter (Free (StoreInstruction instruction f)) encoding =
    let (address, encoding') = pushInstruction encoding instruction
    in  interpreter (f address) encoding'
interpreter (Free (StoreAddressInstruction ref f g)) encoding =
    let (address, encoding') = pushRefInstruction ref encoding f
    in  interpreter (g address) encoding'
interpreter (Free (AddRoutine f g)) encoding =
    let (ref, encoding') = pushRefRourine encoding f
    in  interpreter (g ref) encoding'

_addReturn :: Encoding -> Encoding
_addReturn encoding =
    let
        address = encodingLastAddress encoding
        encoding' =
            encoding
                { encodingMemory =
                    storeInstruction address Return $ encodingMemory encoding
                , encodingLastAddress = address + 2
                }
    in
        encoding'

pushRefRourine :: Encoding -> AssemblyF () -> (Ref, Encoding)
pushRefRourine encoding routine =
    let
        ref = encodingLastReference encoding
        encoding' =
            encoding
                { encodingLastReference = encodingLastReference encoding + 1
                , encodingTargets = Map.insert ref (Right routine) $ encodingTargets encoding
                }
    in
        (ref, encoding')

-- | Push a reference to a sprite into the encoding. This will leave a hole in the memory
-- that will be filled with the instruction when the address to complete the instruction
-- is known.
pushRefInstruction :: Ref -> Encoding -> (Address -> Instruction) -> (Address, Encoding)
pushRefInstruction ref encoding f =
    let address = encodingLastAddress encoding
        encoding' =
            encoding
                { encodingLastAddress = address + 2
                , encodingReferences =
                    Map.insertWith (<>) ref [(address, f)]
                        $ encodingReferences encoding
                }
    in  (address, encoding')

-- | Push a sprite into the encoding. The sprite is not stored in memory yet, but a reference
-- to it is returned while the sprite is stored in the encoding.
pushRefSprite :: Encoding -> Sprite -> (Ref, Encoding)
pushRefSprite encoding sprite' =
    let ref = encodingLastReference encoding
        encoding' =
            encoding
                { encodingLastReference = encodingLastReference encoding + 1
                , encodingTargets =
                    Map.insert ref (Left sprite') $ encodingTargets encoding
                }
    in  (ref, encoding')

-- | Push an instruction into memory and return the address of it. This instruction is not
-- needing a sprite address to be completed.
pushInstruction :: Encoding -> Instruction -> (Address, Encoding)
pushInstruction encoding instruction =
    let address = encodingLastAddress encoding
        encoding' =
            encoding
                { encodingLastAddress = encodingLastAddress encoding + 2
                , encodingMemory =
                    storeInstruction address instruction
                        $ encodingMemory encoding
                }
    in  (address, encoding')

-- | Store a sprite in memory.
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

-- | Store an instruction in memory.
storeInstruction :: Address -> Instruction -> Memory -> Memory
storeInstruction address instruction =
    case opcodeBytes $ encode instruction of
        (h, l') -> Map.insert address h . Map.insert (address + 1) l'

-- | Offload the memory of an encoding into a bytestring.
offloadMemory :: Memory -> ByteString
offloadMemory = pack . stream memoryOffset . Map.assocs
  where
    stream :: Address -> [(Address, Byte)] -> [Word8]
    stream _ [] = []
    stream t ((a, Byte b) : xs) =
        [0 | _ <- [(t + 1) .. a]]
            <> [b]
            <> stream (a + 1) xs

-- | Create an empty encoding where the first address is the given one.
encodingBoot :: Address -> Encoding
encodingBoot address =
    Encoding
        { encodingMemory = Map.empty
        , encodingLastAddress = address
        , encodingLastReference = 0
        , encodingReferences = Map.empty
        , encodingTargets = Map.empty
        , routineAddresses = Map.empty
        }

-------------------------------------- combinators --------------------------------------

-- | A free monad to build a list of elements.
data List x a where
    Elem :: x -> a -> List x a
    deriving (Functor)

-- | Convert a free monad of a list to a list.
list :: Free (List x) a -> [x]
list (Pure _) = []
list (Free (Elem x f)) = x : list f

-- | Add an element to the list.
el :: x -> Free (List x) ()
el x = liftF $ Elem x ()

--------------------- sprite definition support ---------------------

-- | Convert a string to a list of booleans.
line :: String -> [Bool]
line = fmap (/= ' ')

-- | Add a line to the sprite.
l :: String -> Free (List [Bool]) ()
l = el . line

-- | Add a sprite to the program.
sprite :: Free (List [Bool]) a -> Free Assembly Ref
sprite = addSprite . list

-- | A line of an IBM sprite is always followed by an empty line
ls :: String -> Free (List [Bool]) ()
ls x = l x >> l (replicate 8 ' ')

-- | Last line of a sprite. We could use 'l' but it would not align with `ls`
le :: String -> Free (List [Bool]) ()
le = l

--------------------- instruction definition support ---------------------

-- | Add an instruction to the program, discarding the address.
i_ :: Instruction -> Free Assembly ()
i_ = addInstruction_

-- | Add an instruction to the program.
i :: Instruction -> Free Assembly Address
i = addInstruction

-- | Add an instruction that references a sprite to the program.
ia :: Ref -> (Address -> Instruction) -> Free Assembly Address
ia = addAddressInstruction

-- | Add an instruction that references a sprite to the program
-- discarding the address of the instruction.
ia_ :: Ref -> (Address -> Instruction) -> Free Assembly ()
ia_ = addAddressInstruction_

-- | Helper to set the index register to point to a sprite.
si :: Address -> Instruction
si = SetIndexRegister

routine :: AssemblyF () -> AssemblyF Ref
routine = addRoutine

ret :: Free Assembly ()
ret = i_ Return

call :: Ref -> Free Assembly ()
call x = ia_ x Call

--------------------- example ---------------------

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
    ia_ s1 si
    -- draw the circle
    i_ $ Display 0 0 8
    -- set the y coordinate of the cross
    i_ $ SetRegister 1 9
    -- point to the cross sprite
    ia_ s2 si
    -- draw the cross
    i_ $ Display 0 1 8

-- >>> interpreter example $ Encoding mempty 0 0 mempty mempty

encodingExample :: Encoding
encodingExample = interpreter example $ encodingBoot 0

exampleProgram :: ByteString
exampleProgram = offloadMemory $ encodingMemory encodingExample

{- testProgram :: IO ()
testProgram = playGame . game . bootState $ exampleProgram

testInterpreter :: IO ()
testInterpreter =
    pPrint
        $ interpreter example
        $ Encoding mempty memoryOffset 0 mempty mempty mempty
 -}