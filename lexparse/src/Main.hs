module Main where

import Foreign.C.Types

add :: CInt -> CInt -> CInt
add x y = x + y

main :: IO ()
main = pure ()

foreign export ccall add :: CInt -> CInt -> CInt