module Lazy where

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

isEven' 0 = True
isEven' i = isOdd' (i - 1)

isOdd' 0 = False
isOdd' i = isEven' (i - 1)
  
boolToInt :: Bool -> Int
boolToInt True = 42
boolToInt False = 0

main :: Int
main = let a = takeWhile (< 5) [1,1,1,1,5,6]
           b = sum a * 2
           c = sum (a ++ a)
       in boolToInt (b == c)

