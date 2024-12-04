module Map where

import Prelude()
import NanoPrelude

newtype Number = Number Int

addOne :: Number -> Number
addOne (Number n) = Number (n + 1)

main :: Int
main = foldr' (+) 0 (map (+ 1) [1..48]) -- [1,2,3,4,5,6]
