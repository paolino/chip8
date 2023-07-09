module Programs.Dump (dumpPrograms) where

import Data.ByteString (ByteString, writeFile)
import Encoding
    ( AssemblyF
    , Encoding (Encoding, encodingMemory)
    , interpreter
    , offloadMemory
    )
import Offset (memoryOffset)
import qualified Programs.IBM as IBM
import Prelude hiding (writeFile)

encode :: AssemblyF () -> ByteString
encode program =
    offloadMemory
        $ encodingMemory
        $ interpreter program
        $ Encoding mempty memoryOffset 0 mempty mempty

dumpPrograms :: IO ()
dumpPrograms = do
    writeFile "roms/IBM Logo'.ch8" $ encode IBM.program