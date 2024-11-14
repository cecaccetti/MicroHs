module Map where

import Prelude()
import NanoPrelude

main :: Int
main = foldr (+) 0 (map (+ 1) [1,2,3,4,5,6])
