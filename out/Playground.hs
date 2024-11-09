module Playground where

import Prelude()
import NanoPrelude

--isEven :: Int -> Bool
--isEven n =
  --let
    --isEven' 0 = True
 --   isEven' i = isOdd' (i - 1)
   -- isOdd' 0 = False
    --isOdd' i = isEven' (i - 1)
  --in
    --isEven' n
    
add :: Int -> Int -> Int
add a b = a + b

addOne :: Int -> Int
addOne = add 1

main :: Int
main = addOne 41
