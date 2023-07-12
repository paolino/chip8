module Programs.Subroutine (program) where

import Encoding
    ( AssemblyF
    , call
    , le
    , ls
    , ret
    , routine
    , sprite
    )
import Programs.IBM (placeSprite_)

program :: AssemblyF ()
program = do
    s <- sprite $ do
        ls "████████"
        ls "████████"
        ls "  ████  "
        ls "  ████  "
        ls "  ████  "
        ls "  ████  "
        ls "████████"
        le "████████"
    let w = placeSprite_ 1 1 15 s
    w
    r <- routine $ w >> ret
    call r
    call r
