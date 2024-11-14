module LetRec where

import Prelude()
import NanoPrelude

isEven :: Int -> Bool
isEven n = isEven' n
  where
    isEven' 0 = True
    isEven' n = isOdd (n - 1)
    isOdd 0 = False
    isOdd n = isEven' (n - 1)


main :: Bool
main = isEven 4
