module Programs.Dump (dumpPrograms) where

import Data.ByteString (ByteString, writeFile)
import Encoding
    ( AssemblyF
    , Encoding (Encoding, encodingMemory)
    , interpreter
    , offloadMemory
    )
import Offset (memoryOffset)
import Programs.IBM qualified as IBM
import Programs.Subroutine qualified as Subroutine
import Prelude hiding (writeFile)

encode :: AssemblyF () -> ByteString
encode program =
    offloadMemory
        $ encodingMemory
        $ interpreter program
        $ Encoding mempty memoryOffset 0 mempty mempty mempty

dumpPrograms :: IO ()
dumpPrograms = do
    -- writeFile "roms/IBM Logo'.ch8" $ encode IBM.program
    writeFile "roms/Subroutine.ch8" $ encode Subroutine.program
