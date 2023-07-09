module Offset (memoryOffset, addHeaderSpace) where

import Types (Address, Byte)

memoryOffset :: Address
memoryOffset = 512

addHeaderSpace :: [Byte] -> [Byte]
addHeaderSpace = (replicate (fromIntegral memoryOffset) 0 <>)