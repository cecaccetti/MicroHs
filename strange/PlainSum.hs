module PlainSum where

import Prelude()
import NanoPrelude

main :: Int
main = sum (map maximum (replicate 50 [1,2]))
