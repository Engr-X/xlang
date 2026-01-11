module Main where

import Foreign.C.Types

add :: CInt -> CInt -> CInt
add = (+)

main :: IO ()
main = pure ()