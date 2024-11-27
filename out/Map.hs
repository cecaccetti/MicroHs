module Map where

import Prelude()
import NanoPrelude

main :: Int
main = maximum [1..24]-- foldr (+) 0 (map (+ 1) [1..48]) -- [1,2,3,4,5,6]
