module Let where

import Prelude()
import NanoPrelude

isEven :: Int -> Bool
isEven n =
  let
    isEven' 0 = True
    isEven' i = isOdd' (i - 1)
    isOdd' 0 = False
    isOdd' i = isEven' (i - 1)
  in
    isEven' n

main :: Bool
main = isEven 5
