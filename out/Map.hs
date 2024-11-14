module Map where

import Prelude()
import NanoPrelude

main :: Int
--main = fib 4 -- 0 1 1 2 3
--main = apply id 42
--main = foldl and T [T, T, T]
main = foldl (+) 0 [1,2,3,4,5,6]
-- main = sum [1, 2, 3, 4]
-- main = sum []
